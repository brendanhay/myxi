-module(poxy_reader).

-include("include/amqpoxy.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/4]).

%% Callbacks
-export([init/3]).

%% AMQP frame sizes
-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

%% TCP connection timeout
-define(TIMEOUT, 1000).

%% Connection tuning
-define(FRAME_MAX_SIZE, 131072).

-record(v1, {sock                  :: inet:socket(),
             stage = handshake     :: poxy_parser:stage(),
             protocol              :: rabbit_framing:protocol() | undefined,
             info                  :: poxy_parser:payload_info() | undefined,
             proxy    = ""         :: string(),
             pending  = false      :: boolean(),
             buf      = []         :: iolist(),
             buf_len  = 0          :: non_neg_integer(),
             recv_len = ?HANDSHAKE :: non_neg_integer(),
             node                  :: node() | undefined,
             user     = <<"">>     :: binary(),
             conn                  :: pid() | undefined,
             chan                  :: pid() | undefined,
             frame_state}).

%%
%% API
%%

-spec start_link(pid(), inet:socket(),
                 cowboy_tcp_transport, options()) -> {ok, pid()}.
%% @doc
start_link(Listener, Sock, cowboy_tcp_transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Listener, Sock, Opts]),
    {ok, Pid}.

%%
%% Callbacks
%%

-spec init(pid(), inet:socket(), options()) -> ok.
%% @hidden
init(Listener, Sock, Opts) ->
    ok = cowboy:accept_ack(Listener),
    read(advance(#v1{sock = Sock, proxy = amqpoxy:format_ip(Opts)}, handshake)).

%%
%% Private
%%

read(State = #v1{pending = true}) ->
    recv(State);
read(State = #v1{sock = Sock, recv_len = RecvLen, buf_len = BufLen})
  when BufLen < RecvLen ->
    ok = rabbit_net:setopts(Sock, [{active, once}]),
    recv(State#v1{pending = true});
read(State = #v1{recv_len = RecvLen, buf = Buf, buf_len = BufLen}) ->
    {Data, Rest} = split_buffer(Buf, RecvLen),
    NewState = frame(State, Data),
    read(NewState#v1{buf = [Rest], buf_len = BufLen - RecvLen}).

recv(State = #v1{sock = Sock, buf = Buf, buf_len = BufLen}) ->
    case rabbit_net:recv(Sock) of
        {data, Data}    ->
            read(State#v1{buf = [Data|Buf],
                          buf_len = BufLen + size(Data),
                          pending = false});
        closed ->
            throw(connection_closed_abruptly);
        {error, Reason} ->
            throw({inet_error, Reason});
        {other, Other} ->
            lager:info("OTHER ~p", [Other]),
            read(State)
%            other(Other, State)
    end.

split_buffer(Buf, RecvLen) ->
    split_binary(case Buf of
                     [B]    -> B;
                     _Other -> list_to_binary(lists:reverse(Buf))
                 end,
                 RecvLen).

frame(State = #v1{sock = Sock, stage = handshake}, Data) ->
    case poxy_parser:handshake(Data) of
        {start, Start, Protocol} ->
            reply(Sock, Start, Protocol),
            advance(State#v1{protocol = Protocol}, frame_header);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end;

frame(State = #v1{sock = Sock, stage = frame_header}, Data) ->
    case poxy_parser:header(Data) of
        {info, Info} ->
            advance(State#v1{info = Info}, frame_payload);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end;

frame(State = #v1{sock     = Sock,
                  protocol = Protocol,
                  info     = Info,
                  stage    = frame_payload,
                  frame_state = FrameState}, Data) ->
    NewState =
        case poxy_parser:payload(Info, Data, Protocol, FrameState) of
            {refuse, Version, Error} ->
                reply(Sock, Version),
                exit(Error);
            {state, NewFrameState} ->
                State#v1{frame_state = NewFrameState};
            {method, Method} ->
                method(Method, State);
            {method, Method, NewFrameState} ->
                method(Method, State#v1{frame_state = NewFrameState});
            {method, Method, Content, NewFrameState} ->
                method(Method, Content, State#v1{frame_state = NewFrameState})
        end,
    advance(NewState#v1{info = undefined}, frame_header).

advance(State, handshake) ->
    State#v1{stage = handshake, recv_len = ?HANDSHAKE};
advance(State, frame_header) ->
    State#v1{stage = frame_header, recv_len = ?HEADER};
advance(State = #v1{info = {_Type, _Channel, Size}}, frame_payload) ->
    State#v1{stage = frame_payload, recv_len = ?PAYLOAD(Size)}.

method(#'connection.start_ok'{response = Response},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    log("START-OK", State),
    Tune = #'connection.tune'{channel_max = 0,
                              frame_max   = ?FRAME_MAX_SIZE,
                              heartbeat   = 0},
    ok = reply(Sock, Tune, Protocol),
    State#v1{user = poxy_auth:decode(Response)};

method(#'connection.tune_ok'{frame_max = FrameMax}, State) ->
    log("TUNE-OK", State),
    if FrameMax /= 0 andalso FrameMax < ?FRAME_MIN_SIZE ->
            exit({not_allowed, "frame_max=~w < ~w min size",
                  [FrameMax, ?FRAME_MIN_SIZE]});
       ?FRAME_MAX_SIZE /= 0 andalso FrameMax > ?FRAME_MAX_SIZE ->
            exit({not_allowed, "frame_max=~w > ~w max size",
                  [FrameMax, ?FRAME_MAX_SIZE]});
       true ->
            State
    end;

method(#'connection.open'{virtual_host = VHost},
       State = #v1{sock = Sock, protocol = Protocol, user = User}) ->
    log("CONN-OPEN", State),
    {ok, Conn} = open_connection('rabbit@13inches', User, VHost),
    ok = reply(Sock, #'connection.open_ok'{known_hosts = <<"/">>}, Protocol),
    State#v1{node = 'rabbit@13inches', conn = Conn};

method(#'connection.close'{},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    log("CONN-CLOSE", State),
    ok = reply(Sock, #'connection.close_ok'{}, Protocol),
    State;

method(#'connection.close_ok'{}, State) ->
    log("CONN-CLOSE-OK", State),
    State;

method(#'channel.open'{},
       State = #v1{sock = Sock, conn = Conn, chan = undefined, protocol = Protocol}) ->
    log("CHAN-OPEN", State),
    {ok, Chan} = poxy_channel:start_link(Sock, Conn, Protocol),
    State#v1{chan = Chan, frame_state = {method, Protocol}};
method(#'channel.open'{}, State = #v1{sock = Sock, protocol = Protocol}) ->
    log("CHAN-REOPEN", State),
    throw(chan_reopen),
    State;

method(#'channel.close'{}, State) ->
    %% Send close_ok
    log("CHAN-CLOSE", State),
    throw(chan_close),
    State;

method(#'channel.close_ok'{}, State) ->
    log("CHAN-CLOSE-OK", State),
    throw(chan_close_ok),
    State;

method(Method, State = #v1{chan = Chan}) ->
    poxy_channel:method(Chan, Method),
    State.

method(Method, Content, State = #v1{chan = Chan}) ->
    poxy_channel:method(Chan, Method, Content),
    State.

reply(Sock, Data) ->
    ok = rabbit_net:send(Sock, Data).

reply(Sock, Method, Protocol) ->
    ok = rabbit_writer:internal_send_command(Sock, 0, Method, Protocol).

open_connection(Node, User, VHost) ->
    Params = #amqp_params_direct{username     = User,
                                 node         = Node,
                                 virtual_host = VHost},
    case amqp_connection:start(Params) of
        {ok, Conn} ->
            link(Conn),
            {ok, Conn};
        Error ->
            throw(Error)
    end.

-spec log(atom() | string(), #v1{}) -> ok | {error, lager_not_running}.
%% @private
log(Mode, #v1{proxy = Proxy, sock = Sock, node = undefined}) ->
    lager:info("~s ~s -> ~s", [Mode, peername(Sock), Proxy]);
log(Mode, #v1{proxy = Proxy, sock = Sock, node = Node}) ->
    lager:info("~s ~s -> ~s -> ~s", [Mode, peername(Sock), Proxy, Node]).

-spec peername(inet:socket()) -> string() | disconnected.
%% @private
peername(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, Port}} -> amqpoxy:format_ip(Ip, Port);
        _Error           -> 'DISCONNECT'
    end.
