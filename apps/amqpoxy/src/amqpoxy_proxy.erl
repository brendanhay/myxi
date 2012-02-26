-module(amqpoxy_proxy).
-behaviour(cowboy_protocol).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

%% Callbacks
-export([start_link/4,
         init/3]).

-include("include/amqpoxy.hrl").

-record(state, {listener    :: pid(),
                client      :: inet:socket(),
                backend     :: inet:socket(),
                proxy = ""  :: string(),
                protocol    :: module(),
                next        :: pos_integer(),
                payload     :: undefined | {integer(), pos_integer()},
                replay = [] :: [binary()]}).

-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

-define(TIMEOUT, 6000).

%%
%% API
%%

%% @doc
start_link(Listener, Client, cowboy_tcp_transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Listener, Client, Opts]),
    {ok, Pid}.

%%
%% Callbacks
%%

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

%% @private
handshake(State = #state{client = Client, next = Next, replay = Replay}) ->
    case gen_tcp:recv(Client, Next, ?TIMEOUT) of
        {ok, Data}       -> handle(Data, State#state{replay = [Data|Replay]});
        {error, _Reason} -> terminate(State)
    end.

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

%% @private
connect({Major, Minor, _Revision}, Protocol, State = #state{client = Client}) ->
    message("NEGOTIATING", State),
    Start = #'connection.start'{version_major = Major,
                                version_minor = Minor,
                                %% TODO: rabbit_reader:server_properties(Protocol)
                                server_properties = [],
                                locales = <<"en_US">>},
    ok = rabbit_writer:internal_send_command(Client, 0, Start, Protocol),
    handshake(State#state{protocol = Protocol, next = ?HEADER}).

%% @private
decode(Type, Payload, Protocol) ->
    {method, Method, Fields} =
        rabbit_command_assembler:analyze_frame(Type, Payload, Protocol),
    #'connection.start_ok'{response = Response} =
        Protocol:decode_method_fields(Method, Fields),
    Table = rabbit_binary_parser:parse_table(Response),
    {value, {_, _, Login}} = lists:keysearch(<<"LOGIN">>, 1, Table),
    Login.

%% @private
forward(Backend, State = #state{client = Client}) ->
    NewState = replay(State#state{backend = Backend}),
    inet:setopts(Client, [{active, true}]),
    message("ESTABLISHED", NewState),
    proxy(NewState).

%% @private
replay(State = #state{backend = Backend, replay = [Payload, Header, Handshake]}) ->
    message("REPLAY", State),
    ok = gen_tcp:send(Backend, Handshake),
    receive {tcp, Backend, _Data} -> ok end,
    ok = gen_tcp:send(Backend, [Header, Payload]),
    State#state{replay = []}.

%% @private
proxy(State = #state{backend = Backend, client = Client}) ->
    receive
        {tcp, Client, Data} ->
            gen_tcp:send(Backend, Data),
            proxy(State);
        {tcp, Backend, Data} ->
            gen_tcp:send(Client, Data),
            proxy(State);
        {tcp_closed, Client} ->
            message("CLOSED", State),
            gen_tcp:close(Backend),
            ok;
        {tcp_closed, Backend}->
            message("CLOSED", State),
            gen_tcp:close(Client),
            ok
    end.

-spec terminate(#state{}) -> ok.
%% @private
terminate(State = #state{client = Client, backend = Backend}) ->
    message(terminated, State),
    gen_tcp:close(Client),
    case Backend of
        undefined -> ok;
        _         -> gen_tcp:close(Backend)
    end,
    ok.

message(Mode, #state{proxy = Proxy, client = Client, backend = undefined}) ->
    lager:info("~s ~s -> ~s", [Mode, peername(Client), Proxy]);
message(Mode, #state{proxy = Proxy, client = Client, backend = Backend}) ->
    lager:info("~s ~s -> ~s -> ~s", [Mode, peername(Client), Proxy, peername(Backend)]).

peername(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, Port}} -> amqpoxy:format_ip(Ip, Port);
        _Error           -> disconnected
    end.

