%% @doc
-module(amqpoxy_proxy).
-behaviour(cowboy_protocol).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

%% API
-export([start_link/4]).

%% Callbacks
-export([init/3]).

-include("include/amqpoxy.hrl").

%% AMQP frame sizes
-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

%% TCP connection timeout
-define(TIMEOUT, 6000).

-record(state, {client      :: inet:socket(),
                backend     :: inet:socket() | undefined,
                proxy  = "" :: string(),
                protocol    :: module() | undefined,
                stage       :: pos_integer(),
                payload     :: {integer(), pos_integer()} | undefined,
                replay = [] :: [binary()]}).

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

-spec init(pid(), inet:socket(), options()) -> ok.
%% @hidden
init(Listener, Client, Opts) ->
    ok = cowboy:accept_ack(Listener),
    handshake(#state{client = Client,
                     proxy  = amqpoxy:format_ip(Opts),
                     stage  = ?HANDSHAKE}).

%%
%% Private
%%

-spec handshake(#state{}) -> ok.
%% @private
handshake(State = #state{client = Client, stage = Stage, replay = Replay}) ->
    case gen_tcp:recv(Client, Stage, ?TIMEOUT) of
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
    handshake(State#state{stage = ?PAYLOAD(Len), payload = {Type, Len}});
handle(Data, State = #state{protocol = Protocol, payload = {Type, Len}}) ->
    <<Payload:Len/binary, ?FRAME_END>> = Data,
    forward(decode(Type, Payload, Protocol), State).

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
    handshake(State#state{protocol = Protocol, stage = ?HEADER}).

-spec decode(pos_integer(), binary(), rabbit_framing:protocol()) -> match().
%% @private
decode(Type, Payload, Protocol) ->
    {method, Method, Fields} =
        rabbit_command_assembler:analyze_frame(Type, Payload, Protocol),
    #'connection.start_ok'{response = Response} =
        Protocol:decode_method_fields(Method, Fields),
    Table = rabbit_binary_parser:parse_table(Response),
    {value, {_, _, Login}} = lists:keysearch(<<"LOGIN">>, 1, Table),
    {login, Login}.

-spec forward(match(), #state{}) -> ok.
%% @private
forward(Match = {login, Login}, State = #state{client = Client}) ->
    Backend = amqpoxy_router:match(Match),
    NewState = replay(State#state{backend = Backend}),
    ok = inet:setopts(Client, [{active, true}]),
    log(io_lib:fwrite("ESTABLISHED <~s>", [Login]), NewState),
    proxy(NewState).

-spec replay(#state{}) -> #state{}.
%% @private
replay(State = #state{backend = Backend, replay = [Payload, Header, Handshake]}) ->
    log("REPLAY", State),
    ok = gen_tcp:send(Backend, Handshake),
    receive
        {tcp, Backend, _Data} -> ok
    end,
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
