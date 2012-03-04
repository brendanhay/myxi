%% @doc
-module(poxy_frontend).
-behaviour(cowboy_protocol).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/3]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include("include/poxy.hrl").

-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

-type frame_step()  :: handshake | header | payload.

-type frame_type()  :: ?FRAME_METHOD | ?FRAME_HEADER | ?FRAME_BODY |
                       ?FRAME_OOB_METHOD | ?FRAME_OOB_HEADER | ?FRAME_OOB_BODY |
                       ?FRAME_TRACE | ?FRAME_HEARTBEAT.

-type class_id()    :: rabbit_framing:amqp_class_id().

-type frame_state() :: {method, protocol()} |
                       {content_header, method(), class_id(), protocol()} |
                       {content_body,   method(), non_neg_integer(), class_id(), protocol()}.

-type unframed()    :: {method, method()} |
                       {state, frame_state()} |
                       {method, method(), frame_state()} |
                       {method, method(), #content{}, frame_state()}.

-type buffer()      :: [binary()].

-record(s, {client                :: inet:socket(),
            server                :: inet:socket(),
            protocol              :: protocol() | undefined,
            step = handshake      :: frame_step(),
            framing               :: frame_state() | undefined,
            payload_info          :: {integer(), integer(), integer()} | undefined,
            replay = []           :: replay(),
            buf = []              :: buffer(),
            buf_len = 0           :: non_neg_integer(),
            recv = false          :: true | false,
            recv_len = ?HANDSHAKE :: non_neg_integer()}).

%%
%% API
%%

-spec start_link(pid(), client(), cowboy_tcp_transport, options()) -> {ok, pid()}.
%% @doc
start_link(Listener, Client, cowboy_tcp_transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Listener, Client, Opts]),
    {ok, Pid}.

%%
%% Callbacks
%%

-spec init(pid(), client(), options()) -> no_return().
%% @hidden
init(Listener, Client, Opts) ->
    ok = cowboy:accept_ack(Listener),
    ok = inet:setopts(Client, [{active, false}]),
    advance(#s{client = Client}, handshake).

%%
%% Receive
%%

-spec read(#s{}) -> no_return().
%% @private
read(State = #s{recv = true}) ->
    recv(State);
read(State = #s{recv_len = RecvLen, buf_len = BufLen}) when BufLen < RecvLen ->
    recv(State#s{recv = true});
read(State = #s{recv_len = RecvLen, buf = Buf, buf_len = BufLen}) ->
    {Data, Rest} = split_buffer(Buf, RecvLen),
    NewState = update_replay(Data, State),
    input(Data, NewState#s{buf = [Rest], buf_len = BufLen - RecvLen}).

-spec update_replay(binary(), #s{}) -> #s{}.
%% @private
update_replay(_Data, State) when is_port(State#s.server) ->
    State#s{replay = []};
update_replay(Data, State = #s{replay = Replay}) ->
    State#s{replay = [Data|Replay]}.

-spec recv(#s{}) -> ok | no_return().
%% @private
recv(State = #s{client = Client, buf = Buf, buf_len = BufLen}) ->
    case gen_tcp:recv(Client, 0) of
        {ok, Data} ->
            log("FRONTEND-RECV", State),
            read(State#s{buf     = [Data|Buf],
                         buf_len = BufLen + size(Data),
                         recv    = false});
        {error, closed} ->
            terminate(State);
        Error ->
            throw({tcp_error, Error})
    end.

-spec split_buffer(buffer(), pos_integer()) -> {binary(), binary()}.
%% @private
split_buffer(Buf, RecvLen) ->
    split_binary(case Buf of
                     [B]    -> B;
                     _Other -> list_to_binary(lists:reverse(Buf))
                 end,
                 RecvLen).

%%
%% Sockets
%%

-spec reply(client(), binary()) -> ok.
%% @private
reply(Client, Data) -> gen_tcp:send(Client, Data).

-spec reply(client(), method(), protocol()) -> ok.
%% @private
reply(Client, Method, Protocol) ->
    rabbit_writer:internal_send_command(Client, 0, Method, Protocol).

-spec forward(server() | undefined, binary()) -> ok.
%% @private
forward(Server, Data) when is_port(Server) ->
    gen_tcp:send(Server, Data);
forward(undefined, _Data) ->
    ok.

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

%%
%% Parsing
%%

-spec input(binary(), #s{}) -> no_return().
%% @private
input(Data, State = #s{step = handshake}) ->
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
                refuse({bad_version, A, B, C, D}, State);
            Other ->
                refuse({bad_handshake, Other}, State)
        end,
    advance(connection_start(Version, Protocol, State), header);

input(Data, State = #s{step = header, server = Server}) ->
    case Data of
        <<Type:8, Channel:16, Size:32>> ->
            ok = forward(Server, Data),
            advance(State#s{payload_info = {Type, Channel, Size}}, payload);
        _Other ->
            refuse({bad_header, Data}, State)
    end;

input(Data, State = #s{server = Server, step = payload, payload_info = {Type, Channel, Size}, protocol = Protocol, framing = FrameState}) ->
    NewState =
        case Data of
            <<Payload:Size/binary, ?FRAME_END>> ->
                case unframe(Type, Channel, Payload, Protocol, FrameState) of
                    {method, #'connection.start_ok'{response = Response}} ->
                        connection_start_ok(Response, State);
                    {method, _Method} ->
                        State;
                    {method, _Method, NewFrameState} ->
                        State#s{framing = NewFrameState};
                    {method, _Method, _Content, NewFrameState} ->
                        State#s{framing = NewFrameState};
                    {state, NewFrameState} ->
                        State#s{framing = NewFrameState}
                end;
            _Unknown ->
                refuse({bad_payload, Type, Channel, Size, Data}, State)
        end,
    ok = forward(Server, Data),
    advance(NewState, header).

-spec connection_start(version(), protocol(), #s{}) -> #s{}.
%% @private
connection_start({Major, Minor, _Rev}, Protocol, State = #s{client = Client}) ->
    log("START", State),
    Start = #'connection.start'{version_major     = Major,
                                version_minor     = Minor,
                                server_properties = properties(Protocol),
                                locales           = <<"en_US">>},
    ok = reply(Client, Start, Protocol),
    State#s{protocol = Protocol, framing = {method, Protocol}}.

-spec connection_start_ok(binary(), #s{}) -> #s{}.
%% @private
connection_start_ok(Response, State = #s{client = Client, replay = Replay}) ->
    log("START-OK", State),
    User = poxy_auth:user(Response, State#s.protocol),
    {ok, _Pid, Server} = poxy_backend:start_link(Client, User, Replay),
    State#s{server = Server, replay = []}.

-spec advance(#s{}, frame_step()) -> no_return().
%% @private
advance(State, handshake) ->
    read(State#s{step = handshake, recv_len = ?HANDSHAKE});
advance(State, header) ->
    read(State#s{step = header, recv_len = ?HEADER});
advance(State = #s{payload_info = {_Type, _Channel, Size}}, payload) ->
    read(State#s{step = payload, recv_len = ?PAYLOAD(Size)}).

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

-spec refuse(any(), #s{}) -> no_return().
%% @private
refuse(Error, State = #s{client = Client}) ->
    catch reply(Client, <<"AMQP", 0, 0, 9, 1>>),
    lager:error("FRONTEND-ERR ~p", [Error]),
    terminate(State).

-spec terminate(#s{}) -> no_return().
%% @private
terminate(State = #s{server = Server, client = Client}) ->
    log("FRONTEND-CLOSED", State),
    catch forward(Server, <<"AMQP", 0, 0, 9, 1>>),
    catch gen_tcp:close(Server),
    catch gen_tcp:close(Client),
    exit(normal).

-spec unframe(frame_type(), non_neg_integer(), binary(), protocol(),
              frame_state()) -> unframed().
%% @private
unframe(Type, 0, Payload, Protocol, _FrameState) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        {method, Method, Fields} ->
            {method, Protocol:decode_method_fields(Method, Fields)};
        heartbeat ->
            throw(heartbeat_not_supported);
        error ->
            throw({unknown_frame, 0, Type, Payload});
        Unknown ->
            throw({unknown_frame, 0, Type, Payload, Unknown})
    end;

unframe(Type, Chan, Payload, Protocol, FrameState) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        heartbeat ->
            throw(heartbeat_not_supported);
        error ->
            throw({unknown_frame, Chan, Type, Payload});
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
        {error, Reason} ->
            throw({channel_frame, Reason})
    end.
