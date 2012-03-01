-module(poxy_reader).

-include("include/amqpoxy.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

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

-define(FRAME_MAX_SIZE, 131072).

-record(v1, {sock              :: inet:socket(),
             backend           :: node(),
             stage = handshake :: poxy_parser:stage(),
             protocol          :: rabbit_framing:protocol() | undefined,
             info              :: poxy_parser:payload_info() | undefined,
             proxy    = ""     :: string(),
             pending  = false  :: boolean(),
             buf      = []     :: iolist(),
             buf_len  = 0      :: non_neg_integer(),
             recv_len = ?HANDSHAKE :: non_neg_integer()}).

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
    read(next_stage(#v1{sock = Sock, proxy = amqpoxy:format_ip(Opts)}, handshake)).

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
            next_stage(State#v1{protocol = Protocol}, frame_header);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end;

handle(State = #v1{sock = Sock, stage = frame_header}, Data) ->
    case poxy_parser:header(Data) of
        {info, Info} ->
            next_stage(State#v1{info = Info}, frame_payload);
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
            next_stage(NewState#v1{info = undefined}, frame_header);
        {refuse, Version, Error} ->
            reply(Sock, Version),
            exit(Error)
    end.

next_stage(State, handshake) ->
    State#v1{stage = handshake, recv_len = ?HANDSHAKE};
next_stage(State, frame_header) ->
    State#v1{stage = frame_header, recv_len = ?HEADER};
next_stage(State = #v1{info = {_Type, _Channel, Size}}, frame_payload) ->
    State#v1{stage = frame_payload, recv_len = ?PAYLOAD(Size)}.

reply(Sock, Data) ->
    ok = rabbit_net:send(Sock, Data).

reply(Sock, Command, Protocol) ->
    ok = rabbit_writer:internal_send_command(Sock, 0, Command, Protocol).

method(#'connection.start_ok'{response = _Response},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    Tune = #'connection.tune'{channel_max = 0,
                              frame_max = ?FRAME_MAX_SIZE,
                              heartbeat = 0},
    ok = reply(Sock, Tune, Protocol),
    State;

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

method(#'connection.open'{virtual_host = _VHostPath},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    ok = reply(Sock, #'connection.open_ok'{}, Protocol),
    State;

method(#'connection.close'{},
       State = #v1{sock = Sock, protocol = Protocol}) ->
    ok = reply(Sock, #'connection.close_ok'{}, Protocol),
    State;

method(#'connection.close_ok'{}, State) ->
    State;

method(Command, State) ->

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
