%% @doc
-module(poxy_frontend).
-behaviour(cowboy_protocol).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/4]).

-include("include/poxy.hrl").

-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

-type frame_step()     :: handshake | header | payload.

-type frame_type()     :: ?FRAME_METHOD | ?FRAME_HEADER | ?FRAME_BODY |
                          ?FRAME_OOB_METHOD | ?FRAME_OOB_HEADER | ?FRAME_OOB_BODY |
                          ?FRAME_TRACE | ?FRAME_HEARTBEAT.

-type frame_class_id() :: rabbit_framing:amqp_class_id().

-type frame_state()    :: {method, protocol()} |
                          {content_header, method(), frame_class_id(), protocol()} |
                          {content_body, method(), non_neg_integer(), frame_class_id(), protocol()}.

-type unframed()       :: {method, method()} |
                          {state, frame_state()} |
                          {method, method(), frame_state()} |
                          {method, method(), #content{}, frame_state()}.

-type buffer()         :: [binary()].

-record(s, {backend               :: pid(),
            client                :: inet:socket(),
            server                :: inet:socket(),
            protocol              :: protocol() | undefined,
            router                :: module(),
            interceptors          :: [interceptor()],
            step = handshake      :: frame_step(),
            framing               :: frame_state() | undefined,
            payload_info          :: {binary(), integer(), integer(), integer()} | undefined,
            replay = []           :: iolist(),
            buf = []              :: buffer(),
            buf_len = 0           :: non_neg_integer(),
            recv = false          :: true | false,
            recv_len = ?HANDSHAKE :: non_neg_integer()}).

%%
%% API
%%

%% @doc
-spec start_link(pid(), client(), cowboy_tcp_transport, frontend())
                -> {ok, pid()} | ignore | {error, _}.
start_link(Listener, Client, cowboy_tcp_transport, Config) ->
    proc_lib:start_link(?MODULE, init, [self(), Listener, Client, Config]).

%%
%% Callbacks
%%

%% @hidden
init(Parent, Listener, Client, Config) ->
    lager:info("FRONTEND-INIT ~p", [self()]),
    {ok, Backend} = poxy_backend:start_link(self(), Client),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    ok = cowboy:accept_ack(Listener),
    State = #s{backend = Backend,
               client  = Client,
               router  = poxy_router:new(Config)},
    next_state(State, handshake).

%%
%% Receive
%%

-spec accumulate(#s{}) -> no_return().
%% @private
accumulate(State = #s{recv = true}) ->
    read(State);
accumulate(State = #s{recv_len = RecvLen, buf_len = BufLen}) when BufLen < RecvLen ->
    read(State#s{recv = true});
accumulate(State = #s{recv_len = RecvLen, buf = Buf, buf_len = BufLen}) ->
    {Data, Rest} = split_buffer(Buf, RecvLen),
    NewState = update_replay(Data, State),
    parse(Data, NewState#s{buf = [Rest], buf_len = BufLen - RecvLen}).

-spec read(#s{}) -> ok | no_return().
%% @private
read(State = #s{client = Client, buf = Buf, buf_len = BufLen}) ->
    case gen_tcp:recv(Client, 0) of
        {ok, Data} ->
            accumulate(State#s{buf     = [Data|Buf],
                               buf_len = BufLen + size(Data),
                               recv    = false});
        {error, closed} ->
            disconnect(client_closed, State);
        Error ->
            disconnect(Error, State)
    end.

-spec update_replay(binary(), #s{}) -> #s{}.
%% @private
update_replay(_Data, State) when is_pid(State#s.server) ->
    State#s{replay = []};
update_replay(Data, State = #s{replay = Replay}) ->
    State#s{replay = [Data|Replay]}.

-spec split_buffer(buffer(), pos_integer()) -> {binary(), binary()}.
%% @private
split_buffer(Buf, RecvLen) ->
    split_binary(case Buf of
                     [B]    -> B;
                     _Other -> list_to_binary(lists:reverse(Buf))
                 end,
                 RecvLen).

-spec next_state(#s{}, frame_step()) -> no_return().
%% @private
next_state(State, handshake) ->
    accumulate(State#s{step = handshake, recv_len = ?HANDSHAKE});
next_state(State, header) ->
    accumulate(State#s{step = header, recv_len = ?HEADER});
next_state(State = #s{payload_info = {_Header, _Type, _Channel, Size}}, payload) ->
    accumulate(State#s{step = payload, recv_len = ?PAYLOAD(Size)}).

-spec disconnect(any(), #s{}) -> no_return().
%% @private
disconnect(Error, #s{backend = Backend, server = Server, client = Client}) ->
    lager:error("FRONTEND-ERR ~p", [Error]),
    poxy:disconnect([Server, Client]),
    exit(Backend, frontend_disconnect),
    exit(normal).

%%
%% Handshake
%%

-spec connection_start(version(), protocol(), #s{}) -> #s{}.
%% @private
connection_start({Major, Minor, _Rev}, Protocol, State = #s{client = Client}) ->
    log("START", State),
    Start = #'connection.start'{version_major     = Major,
                                version_minor     = Minor,
                                server_properties = properties(Protocol),
                                locales           = <<"en_US">>},
    ok = poxy_writer:send(Client, 0, Start, Protocol),
    State#s{protocol = Protocol, framing = {method, Protocol}}.

-spec properties(rabbit_framing:protocol()) -> rabbit_framing:amqp_table().
%% @private
properties(Protocol) ->
    [{<<"capabilities">>, table,   capabilities(Protocol)},
     {<<"product">>,      longstr, <<"Poxy">>},
     {<<"version">>,      longstr, <<"0.0.1">>},
     {<<"platform">>,     longstr, <<"Erlang/OTP">>},
     {<<"copyright">>,    longstr, <<"">>},
     {<<"information">>,  longstr, <<"">>}].

-spec capabilities(rabbit_framing:protocol()) -> [{binary(), bool, true}].
%% @private
capabilities(rabbit_framing_amqp_0_9_1) ->
    [{<<"publisher_confirms">>,         bool, true},
     {<<"exchange_exchange_bindings">>, bool, true},
     {<<"basic.nack">>,                 bool, true},
     {<<"consumer_cancel_notify">>,     bool, true}];
capabilities(_) ->
    [].

-spec connection_start_ok(binary(), #s{}) -> #s{}.
%% @private
connection_start_ok(StartOk, State = #s{backend  = Backend,
                                        router   = Router,
                                        replay   = Replay,
                                        protocol = Protocol}) ->
    log("START-OK", State),
    {Addr, Inters} = poxy_router:route(Router, StartOk, Protocol),
    {ok, Server} = poxy_backend:connect(Backend, Addr, Replay),
    State#s{server = Server, interceptors = Inters, replay = []}.

%%
%% Parsing
%%

-spec parse(binary(), #s{}) -> no_return().
%% @private
parse(Data, State = #s{step = handshake}) ->
    {Version, Protocol} =
        case Data of
            <<"AMQP", 0, 0, 9, 1>> ->
                {{0, 9, 1}, rabbit_framing_amqp_0_9_1};
            <<"AMQP", 1, 1, 0, 9>> ->
                {{0, 9, 0}, rabbit_framing_amqp_0_9_1};
            <<"AMQP", 1, 1, 8, 0>> ->
                {{8, 0, 0}, rabbit_framing_amqp_0_8};
            <<"AMQP", 1, 1, 9, 1>> ->
                {{8, 0, 0}, rabbit_framing_amqp_0_8};
            <<"AMQP", A, B, C, D>> ->
                disconnect({bad_version, A, B, C, D}, State);
            Other ->
                disconnect({bad_handshake, Other}, State)
        end,
    next_state(connection_start(Version, Protocol, State), header);

parse(Data, State = #s{step = header}) ->
    case Data of
        <<Type:8, Channel:16, Size:32>> ->
            next_state(State#s{payload_info = {Data, Type, Channel, Size}}, payload);
        _Other ->
            disconnect({bad_header, Data}, State)
    end;

parse(Data, State = #s{step         = payload,
                       interceptors = Inters,
                       payload_info = {Header, Type, Channel, Size},
                       protocol     = Protocol,
                       framing      = FrameState}) ->
    {NewMethod, NewState} =
        case Data of
            <<Payload:Size/binary, ?FRAME_END>> ->
                case unframe(Type, Channel, Payload, Protocol, FrameState) of
                    {method, StartOk = #'connection.start_ok'{}} ->
                        {ignore, connection_start_ok(StartOk, State)};
                    {method, Method} ->
                        {Method, State};
                    {method, Method, NewFrameState} ->
                        {Method, State#s{framing = NewFrameState}};
                    {method, Method, _Content, NewFrameState} ->
                        {Method, State#s{framing = NewFrameState}};
                    {state, NewFrameState} ->
                        {passthrough, State#s{framing = NewFrameState}};
                    Error ->
                        disconnect(Error, State)
                end;
            _Unknown ->
                disconnect({bad_payload, Type, Channel, Size, Data}, State)
        end,
    ok = poxy_writer:forward(NewState#s.server, [Header, Data], Channel,
                             NewMethod, Protocol, Inters),
    next_state(NewState, header).

%%
%% Framing
%%

-spec unframe(frame_type(), non_neg_integer(), binary(), protocol(),
              frame_state()) -> unframed().
%% @private
unframe(Type, 0, Payload, Protocol, _FrameState) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        {method, Method, Fields} ->
            {method, Protocol:decode_method_fields(Method, Fields)};
        heartbeat ->
            heartbeat_not_supported;
        error ->
            {unknown_frame, 0, Type, Payload};
        Unknown ->
            {unknown_frame, 0, Type, Payload, Unknown}
    end;

unframe(Type, Chan, Payload, Protocol, FrameState) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        heartbeat ->
            heartbeat_not_supported;
        error ->
            {unknown_frame, Chan, Type, Payload};
        {method, Method, <<0>>} ->
            {method, Protocol:decode_method_fields(Method, <<0>>)};
        Frame ->
            channel_unframe(Frame, FrameState)
    end.

-spec channel_unframe(any(), frame_state()) -> unframed().
%% @private
channel_unframe(Current, Previous) ->
    case rabbit_command_assembler:process(Current, Previous) of
        {ok, NewFrameState} ->
            {state, NewFrameState};
        {ok, Method, NewFrameState} ->
            {method, Method, NewFrameState};
        {ok, Method, Content, NewFrameState} ->
            {method, Method, Content, NewFrameState};
        Error ->
            Error
    end.

%%
%% Logging
%%

-spec log(string() | atom(), #s{}) -> ok.
%% @private
log(Mode, #s{client = Client, server = undefined}) ->
    lager:info("~s ~s -> ~p",
               [Mode, poxy:peername(Client), self()]);
log(Mode, #s{client = Client, server = Server}) ->
    lager:info("~s ~s -> ~p -> ~s",
               [Mode, poxy:peername(Client), self(), poxy:peername(Server)]).
