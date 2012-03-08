-module(totochtin_connection_fsm).

-behaviour(gen_fsm).
-behaviour(cowboy_protocol).

-include("include/totochtin.hrl").

%% API
-export([start_link/4,
         reply/2,
         reply/4,
         replay/4,
         forward/5]).

%% Callbacks
-export([init/1,
         handshake/2,
         handshake/3,
         proxy/2,
         proxy/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(s, {router   :: router(),
            listener :: pid(),
            backend  :: pid(),
            frontend :: pid(),
            client   :: inet:socket(),
            server   :: inet:socket(),
            policies :: [policy()]}).

%%
%% API
%%

start_link(Listener, Client, cowboy_tcp_transport, Config) ->
    case gen_fsm:start_link(?MODULE, {Listener, Client, Config}, []) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Client, Pid),
            {ok, Pid};
        Error ->
            Error
    end.

reply(Conn, Raw) ->
    gen_fsm:send_event(Conn, {reply, Raw}).

reply(Conn, Channel, Method, Protocol) ->
    gen_fsm:send_event(Conn, {reply, Channel, Method, Protocol}).

replay(Conn, StartOk, Replay, Protocol) ->
    gen_fsm:sync_send_event(Conn, {replay, StartOk, Replay, Protocol}).

forward(Conn, Raw, Channel, Method, Protocol) ->
    gen_fsm:send_event(Conn, {forward, Raw, Channel, Method, Protocol}).

%%
%% Callbacks
%%

-spec init(_) -> {ok, #s{}}.
%% @hidden
init({Listener, Client, Config}) ->
    process_flag(trap_exit, true),
    lager:info("CONN-INIT ~p", [Client]),
    totochtin_stats:connected(self()),
    {ok, idle, #s{router   = totochtin_router:new(Config),
                  listener = Listener,
                  client   = Client}}.

handshake({reply, Channel, Method, Protocol}, State) ->
    lager:info("FSM-HANDSHAKE"),
    client_reply(Channel, Method, Protocol, State),
    {next_state, handshake, State}.

handshake({replay, StartOk, Replay, Protocol}, From,
          State = #s{router = Router, frontend = Frontend}) ->
    lager:info("FSM-HANDSHAKE-REPLAY"),
    {Addr, Policies} = totochtin_router:route(Router, StartOk, Protocol),
    {ok, Backend, Server} = totochtin_backend:start_link(self(), Addr, Replay),
    {reply, ok, proxy, State#s{backend  = Backend,
                               server   = Server,
                               policies = Policies}}.

proxy({reply, Raw}, State) ->
    client_reply(Raw, State),
    {next_state, proxy, State};
proxy({forward, Raw, Channel, Method, Protocol}, State) ->
    lager:info("FSM-PROXY-FORWARD"),
    server_forward(Raw, Channel, Method, Protocol, State),
    {next_state, proxy, State}.

proxy(_Event, _From, State) ->
    lager:info("FSM-PROXY/3"),
    {reply, ok, proxy, State}.

handle_event(_Event, StateName, State) ->
    lager:info("EVENT"),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:info("SYNC-EVENT"),
    {reply, ok, StateName, State}.

%% @hidden
handle_info({'EXIT', Pid, _Msg}, _Any, State = #s{frontend = _Pid}) ->
    lager:info("FRONTEND-EXIT ~p", [Pid]),
    {stop, normal, State};
handle_info({'EXIT', Pid, _Msg}, _Any, State = #s{backend = _Pid}) ->
    lager:info("BACKEND-EXIT ~p", [Pid]),
    {stop, normal, State};

%% Cowboy acknowledgement
handle_info({shoot, Listener}, idle,
            State = #s{listener = Listener, client = Client}) ->
    lager:info("FSM-ACCEPT"),
    {ok, Frontend} = totochtin_frontend:start_link(self(), Client),
    {next_state, handshake, State#s{frontend = Frontend}}.

terminate(Reason, _StateName, #s{backend = Backend,
                                  frontend = Frontend,
                                  server = Server,
                                  client = Client}) ->
    lager:info("FSM-EXIT ~p", [Reason]),
    catch exit(Backend, kill),
    catch exit(Frontend, kill),
    catch gen_tcp:close(Server),
    catch gen_tcp:close(Client),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Private
%%

server_forward(_Data, _Channel, ignore, _Protocol, State) ->
    {noreply, State};
server_forward(Data, _Channel, passthrough, _Protocol, State = #s{server = Server}) ->
    send(Server, Data, State);
server_forward(Data, Channel, Method, Protocol,
          State = #s{server = Server, policies = Policies}) ->
    case totochtin_policy:thrush(Method, Policies) of
        {modified, NewMethod} ->
            lager:info("MODIFIED ~p", [NewMethod]),
            ok = rabbit_writer:internal_send_command(Server, 0, Method, Protocol),
            {noreply, State};
        {unmodified, Method} ->
            send(Server, Data, State)
    end.

client_reply(Data, State = #s{client = Client}) ->
    send(Client, Data, State).

client_reply(0, Method, Protocol, State = #s{client = Client}) ->
    ok = rabbit_writer:internal_send_command(Client, 0, Method, Protocol),
    {noreply, State};
client_reply(Channel, Method, Protocol, State = #s{client = Client}) ->
    Frame =
        rabbit_binary_generator:build_simple_method_frame(Channel, Method, Protocol),
    send(Client, Frame, State).

%%-spec send(inet:socket(), binary()) -> ok | {error, _}.
%% @private
send(Sock, Data, State) ->
    lager:info("~p ~p",[Sock,Data]),
    case gen_tcp:send(Sock, Data) of
        ok ->
            ok;
        Error ->
            lager:error("CONN-ERR ~p", [Error])
    end.
