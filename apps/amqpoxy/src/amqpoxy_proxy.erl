%% @doc
-module(amqpoxy_proxy).
-behaviour(cowboy_protocol).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

%% API
-export([start_link/4]).

%% Callbacks
-export([init/3]).

-include("include/amqpoxy.hrl").

-record(state, {listener    :: pid(),
                client      :: inet:socket(),
                backend     :: undefined | inet:socket(),
                proxy = ""  :: string(),
                protocol    :: module(),
                next        :: pos_integer(),
                payload     :: undefined | {integer(), pos_integer()},
                replay = [] :: [binary()]}).

%% AMQP frame sizes
-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

%% TCP connection timeout
-define(TIMEOUT, 6000).

-type version() :: {0 | 8,0 | 9,0 | 1}.

%%
%% API
%%

-spec start_link(pid(), inet:socket(),
                 cowboy_tcp_transport, options()) -> {ok, pid()}.
%% @doc
start_link(Listener, Client, cowboy_tcp_transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Listener, Client, Opts]),
    {ok, Pid}.

%%
%% Callbacks
%%

-spec init(pid(), inet:socket(), options()) -> ok | no_return().
%% @private
init(Listener, Client, Opts) ->
    ok = cowboy:accept_ack(Listener),
    handshake(#state{proxy    = amqpoxy:format_ip(Opts),
                     listener = Listener,
                     client   = Client,
                     next     = ?HANDSHAKE}).

%%
%% Private
%%

-spec handshake(#state{}) -> ok | no_return().
%% @private
handshake(State = #state{client = Client, next = Next, replay = Replay}) ->
    case gen_tcp:recv(Client, Next, ?TIMEOUT) of
        {ok, Data}       -> handle(Data, State#state{replay = [Data|Replay]});
        {error, _Reason} -> terminate(State)
    end.

-spec terminate(#state{}) -> ok.
%% @private
terminate(State = #state{client = Client, backend = undefined}) ->
    log(terminated, State),
    ok = gen_tcp:close(Client).

-spec handle(binary(), #state{}) -> ok.
%% @private
handle(<<"AMQP", 0, 0, 9, 1>>, State) ->
    connect({0, 9, 1}, rabbit_framing_amqp_0_9_1, State);
handle(<<"AMQP", 1, 1, 0, 9>>, State) ->
    connect({0, 9, 0}, rabbit_framing_amqp_0_9_1, State);
handle(<<"AMQP", 1, 1, 8, 0>>, State) ->
    connect({8, 0, 0}, rabbit_framing_amqp_0_8, State);
handle(<<"AMQP", 1, 1, 9, 1>>, State) ->
    connect({8, 0, 0}, rabbit_framing_amqp_0_8, State);
handle(<<Type:8, _:16, Len:32>>, State) ->
    handshake(State#state{next = ?PAYLOAD(Len), payload = {Type, Len}});
handle(Data, State = #state{protocol = Protocol, payload = {Type, Len}}) ->
    <<Payload:Len/binary, ?FRAME_END>> = Data,
    Login = decode(Type, Payload, Protocol),
    forward(amqpoxy_router:match({login, Login}), State).

-spec connect(version(), rabbit_framing:protocol(), #state{}) -> ok.
%% @private
connect({Major, Minor, _Revision}, Protocol, State = #state{client = Client}) ->
    log("NEGOTIATING", State),
    Start = #'connection.start'{version_major = Major,
                                version_minor = Minor,
                                %% TODO: rabbit_reader:server_properties(Protocol)
                                server_properties = [],
                                locales = <<"en_US">>},
    ok = rabbit_writer:internal_send_command(Client, 0, Start, Protocol),
    handshake(State#state{protocol = Protocol, next = ?HEADER}).

-spec decode(pos_integer(), binary(), rabbit_framing:protocol()) -> binary().
%% @private
decode(Type, Payload, Protocol) ->
    {method, Method, Fields} =
        rabbit_command_assembler:analyze_frame(Type, Payload, Protocol),
    #'connection.start_ok'{response = Response} =
        Protocol:decode_method_fields(Method, Fields),
    Table = rabbit_binary_parser:parse_table(Response),
    {value, {_, _, Login}} = lists:keysearch(<<"LOGIN">>, 1, Table),
    Login.

-spec forward(inet:socket(), #state{}) -> ok.
%% @private
forward(Backend, State = #state{client = Client}) ->
    NewState = replay(State#state{backend = Backend}),
    ok = inet:setopts(Client, [{active, true}]),
    log("ESTABLISHED", NewState),
    proxy(NewState).

-spec replay(#state{}) -> #state{}.
%% @private
replay(State = #state{backend = Backend, replay = [Payload, Header, Handshake]}) ->
    log("REPLAY", State),
    ok = gen_tcp:send(Backend, Handshake),
    receive {tcp, Backend, _Data} -> ok end,
    ok = gen_tcp:send(Backend, [Header, Payload]),
    State#state{replay = []}.

-spec proxy(#state{}) -> ok.
%% @private
proxy(State = #state{backend = Backend, client = Client}) ->
    receive
        {tcp, Client, Data} ->
            ok = gen_tcp:send(Backend, Data),
            proxy(State);
        {tcp, Backend, Data} ->
            ok = gen_tcp:send(Client, Data),
            proxy(State);
        {tcp_closed, Client} ->
            log("CLOSED", State),
            gen_tcp:close(Backend),
            ok;
        {tcp_closed, Backend}->
            log("CLOSED", State),
            gen_tcp:close(Client),
            ok
    end.

-spec log(atom() | string(), #state{}) -> ok | {error, lager_not_running}.
%% @private
log(Mode, #state{proxy = Proxy, client = Client, backend = undefined}) ->
    lager:info("~s ~s -> ~s", [Mode, peername(Client), Proxy]);
log(Mode, #state{proxy = Proxy, client = Client, backend = Backend}) ->
    lager:info("~s ~s -> ~s -> ~s", [Mode, peername(Client), Proxy, peername(Backend)]).

-spec peername(inet:socket()) -> string() | disconnected.
%% @private
peername(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, Port}} -> amqpoxy:format_ip(Ip, Port);
        _Error           -> disconnected
    end.
