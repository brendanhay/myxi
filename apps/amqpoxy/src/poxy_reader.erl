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
             backend               :: node(),
             stage = handshake     :: poxy_parser:stage(),
             protocol              :: rabbit_framing:protocol() | undefined,
             info                  :: poxy_parser:payload_info() | undefined,
             proxy    = ""         :: string(),
             pending  = false      :: boolean(),
             buf      = []         :: iolist(),
             buf_len  = 0          :: non_neg_integer(),
             recv_len = ?HANDSHAKE :: non_neg_integer(),
             user                  :: binary() | undefined,
             conn                  :: pid() | undefined,
            chan                   :: pid() | undefined }).

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
    NewState = handle(State, Data),
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
            error({need_to_handle_other, Other, State})
    end.

split_buffer(Buf, RecvLen) ->
    split_binary(case Buf of
                     [B]    -> B;
                     _Other -> list_to_binary(lists:reverse(Buf))
                 end,
                 RecvLen).

handle(State = #v1{sock = Sock, stage = handshake}, Data) ->
    case poxy_parser:handshake(Data) of
        {start, Start, Protocol} ->
            reply(Sock, Start, Protocol),
            advance(State#v1{protocol = Protocol}, frame_header);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end;

handle(State = #v1{sock = Sock, stage = frame_header}, Data) ->
    case poxy_parser:header(Data) of
        {info, Info} ->
            advance(State#v1{info = Info}, frame_payload);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end;

handle(State = #v1{sock     = Sock,
                   protocol = Protocol,
                   info     = Info,
                   stage    = frame_payload}, Data) ->
    case poxy_parser:payload(Info, Data, Protocol) of
        {method, Method} ->
            NewState = method(Method, State),
            advance(NewState#v1{info = undefined}, frame_header);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end.

advance(State, handshake) ->
    State#v1{stage = handshake, recv_len = ?HANDSHAKE};
advance(State, frame_header) ->
    State#v1{stage = frame_header, recv_len = ?HEADER};
advance(State = #v1{info = {_Type, _Channel, Size}}, frame_payload) ->
    State#v1{stage = frame_payload, recv_len = ?PAYLOAD(Size)}.

method(#'connection.start_ok'{response = Response},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    Tune = #'connection.tune'{channel_max = 0,
                              frame_max = ?FRAME_MAX_SIZE,
                              heartbeat = 0},
    ok = reply(Sock, Tune, Protocol),
    State#v1{user = decode_user(Response)};

method(#'connection.tune_ok'{frame_max = FrameMax}, State) ->
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
    {ok, Conn} = open_connection('rabbit@13inches', User, VHost),
    ok = reply(Sock, #'connection.open_ok'{}, Protocol),
    State#v1{conn = Conn};

method(#'connection.close'{},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    ok = reply(Sock, #'connection.close_ok'{}, Protocol),
    State;

method(#'connection.close_ok'{}, State) ->
    State;

method(#'channel.open'{},
       State = #v1{sock = Sock, conn = Conn, chan = undefined, protocol = Protocol}) ->
    {ok, Chan} = open_channel(Conn),
    ok = reply(Sock, #'channel.open_ok'{channel_id = <<"0">>}, Protocol),
    State#v1{chan = Chan};
method(#'channel.open'{}, State = #v1{sock = Sock, protocol = Protocol}) ->
    ok = reply(Sock, #'channel.open_ok'{channel_id = <<"0">>}, Protocol),
    State;

method(#'channel.close'{}, State) ->
    %% Send close_ok
    State;

method(#'channel.close_ok'{}, State) ->
    State;

method(Method, State = #v1{sock = Sock, chan = Chan, protocol = Protocol}) ->
    Res = amqp_channel:call(Chan, Method),
    lager:info("forwarding: ~p", [Res]),
    ok = reply(Sock, Res, Protocol),
    State.

reply(Sock, Data) ->
    ok = rabbit_net:send(Sock, Data).

reply(Sock, Command, Protocol) ->
    ok = rabbit_writer:internal_send_command(Sock, 0, Command, Protocol).

decode_user(Response) ->
    Table = rabbit_binary_parser:parse_table(Response),
    {value, {_, _, User}} = lists:keysearch(<<"LOGIN">>, 1, Table),
    User.

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

open_channel(Conn) ->
    case amqp_connection:open_channel(Conn) of
        {ok, Chan} ->
            link(Chan),
            {ok, Chan};
        Error ->
            catch amqp_connection:close(Conn),
            throw(Error)
    end.

-spec log(atom() | string(), #v1{}) -> ok | {error, lager_not_running}.
%% @private
log(Mode, #v1{proxy = Proxy, sock = Sock, backend = undefined}) ->
    lager:info("~s ~s -> ~s", [Mode, peername(Sock), Proxy]);
log(Mode, #v1{proxy = Proxy, sock = Sock, backend = Backend}) ->
    lager:info("~s ~s -> ~s -> ~s", [Mode, peername(Sock), Proxy, peername(Backend)]).

-spec peername(inet:socket()) -> string() | disconnected.
%% @private
peername(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, Port}} -> amqpoxy:format_ip(Ip, Port);
        _Error           -> 'DISCONNECT'
    end.
