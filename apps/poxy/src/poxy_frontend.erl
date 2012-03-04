%% @doc
-module(poxy_frontend).
-behaviour(cowboy_protocol).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/3]).

-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include("include/poxy.hrl").

-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

-type state()     :: handshake | header | payload.
-type protocol()  :: rabbit_framing:protocol().
-type method()    :: rabbit_framing:amqp_method_record().
-type class_id()  :: rabbit_framing:amqp_class_id().
-type body_size() :: non_neg_integer().
-type framing()   ::
        {method, protocol()} |
        {content_header, method(), class_id(), protocol()} |
        {content_body,   method(), body_size(), class_id(), protocol()}.

-record(s, {client                :: inet:clientet(),
            server                :: inet:clientet(),
            listener = ""         :: string(),
            protocol              :: protocol() | undefined,
            step = handshake      :: state(),
            framing               :: framing() | undefined,
            payload_info          :: {integer(), integer(), integer()} | undefined,
            replay = []           :: iolist(),
            buf = []              :: iolist(),
            buf_len = 0           :: non_neg_integer(),
            recv = false          :: true | false,
            recv_len = ?HANDSHAKE :: non_neg_integer()}).

%%
%% API
%%

-spec start_link(pid(), inet:clientet(),
                 cowboy_tcp_transport, options()) -> {ok, pid()}.
%% @doc
start_link(Listener, Client, cowboy_tcp_transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Listener, Client, Opts]),
    {ok, Pid}.

%%
%% Callbacks
%%

%% @hidden
init(Listener, Client, Opts) ->
    ok = cowboy:accept_ack(Listener),
    ok = rabbit_net:setopts(Client, [{active, once}]),
    advance(#s{client = Client, listener = poxy:format_ip(Opts)}, handshake).

%%
%% Private
%%

read(State = #s{recv = true}) ->
    recv(State);
read(State = #s{recv_len = RecvLen, buf_len = BufLen}) when BufLen < RecvLen ->
    recv(State#s{recv = true});

read(State = #s{recv_len = RecvLen, buf = Buf, buf_len = BufLen}) ->
    {Data, Rest} = split_buffer(Buf, RecvLen),
    NewState = update_replay(Data, State),
    input(Data, NewState#s{buf = [Rest], buf_len = BufLen - RecvLen}).

update_replay(_Data, State) when is_port(State#s.server) ->
    State#s{replay = []};
update_replay(Data, State = #s{replay = Replay}) ->
    State#s{replay = [Data|Replay]}.

recv(State = #s{client = Client, buf = Buf, buf_len = BufLen}) ->
    ok = rabbit_net:setopts(Client, [{active, once}]),
    case rabbit_net:recv(Client) of
        {data, Data} ->
            read(State#s{buf = [Data|Buf],
                         buf_len = BufLen + size(Data),
                         recv = false});
        closed ->
            terminate(State);
        {error, Reason} ->
            throw({inet_error, Reason})
    end.

split_buffer(Buf, RecvLen) ->
    split_binary(case Buf of
                     [B]    -> B;
                     _Other -> list_to_binary(lists:reverse(Buf))
                 end,
                 RecvLen).

reply(Client, Data) ->
    ok = rabbit_net:send(Client, Data).

reply(Client, Method, Protocol) ->
    ok = rabbit_writer:internal_send_command(Client, 0, Method, Protocol).

forward(Server, Data) when is_port(Server) ->
    gen_tcp:send(Server, Data);
forward(undefined, _Data) ->
    ok.

%% @private
log(Mode, #s{listener = Listener, client = Client, server = undefined}) ->
    lager:info("~s ~s -> ~s", [Mode, peername(Client), Listener]);
log(Mode, #s{listener = Listener, client = Client, server = Server}) ->
    lager:info("~s ~s -> ~s -> ~s", [Mode, peername(Client), Listener, peername(Server)]).

-spec peername(inet:clientet()) -> string() | disconnected.
%% @private
peername(Clientet) ->
    case inet:peername(Clientet) of
        {ok, {Ip, Port}} -> poxy:format_ip(Ip, Port);
        _Error           -> 'DISCONNECT'
    end.

%%
%% Parsing
%%

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

connection_start({Major, Minor, _Rev}, Protocol, State = #s{client = Client}) ->
    log("START", State),
    Start = #'connection.start'{version_major     = Major,
                                version_minor     = Minor,
                                server_properties = properties(Protocol),
                                locales           = <<"en_US">>},
    ok = reply(Client, Start, Protocol),
    State#s{protocol = Protocol, framing = {method, Protocol}}.

connection_start_ok(Response, State = #s{client = Client, replay = Replay}) ->
    log("START-OK", State),
    User = poxy_auth:decode(Response),
    {ok, _Pid, Server} = poxy_backend:start_link(Client, User, Replay),
    State#s{server = Server, replay = []}.

advance(State, handshake) ->
    read(State#s{step = handshake, recv_len = ?HANDSHAKE});
advance(State, header) ->
    read(State#s{step = header, recv_len = ?HEADER});
advance(State = #s{payload_info = {_Type, _Channel, Size}}, payload) ->
    read(State#s{step = payload, recv_len = ?PAYLOAD(Size)}).

-spec properties(rabbit_framing:protocol()) -> rabbit_framing:amqp_table().
properties(Protocol) ->
    [{<<"capabilities">>, table,   capabilities(Protocol)},
     {<<"product">>,      longstr, <<"Poxy">>},
     {<<"version">>,      longstr, <<"0.0.1">>},
     {<<"platform">>,     longstr, <<"Erlang/OTP">>},
     {<<"copyright">>,    longstr, <<"">>},
     {<<"information">>,  longstr, <<"">>}].

-spec capabilities(rabbit_framing:protocol()) -> [{binary(), bool, true}].
capabilities(rabbit_framing_amqp_0_9_1) ->
    [{<<"publisher_confirms">>,         bool, true},
     {<<"exchange_exchange_bindings">>, bool, true},
     {<<"basic.nack">>,                 bool, true},
     {<<"consumer_cancel_notify">>,     bool, true}];
capabilities(_) ->
    [].

refuse(Error, State = #s{client = Client}) ->
    catch reply(Client, <<"AMQP", 0, 0, 9, 1>>),
    lager:error("ERROR ~p", [Error]),
    terminate(State).

terminate(#s{server = Server, client = Client}) ->
    catch forward(Server, <<"AMQP", 0, 0, 9, 1>>),
    catch gen_tcp:close(Server),
    catch gen_tcp:close(Client),
    exit(normal).

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

channel_unframe(Frame, FrameState) ->
    case rabbit_command_assembler:process(Frame, FrameState) of
        {ok, NewFrameState} ->
            {state, NewFrameState};
        {ok, Method, NewFrameState} ->
            {method, Method, NewFrameState};
        {ok, Method, Content, NewFrameState} ->
            {method, Method, Content, NewFrameState};
        {error, Reason} ->
            throw({channel_frame, Reason})
    end.
