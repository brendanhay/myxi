%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_connection).

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
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% States
-export([handshaking/2,
         handshaking/3,
         proxying/2,
         closing/2]).

-record(s, {router   :: router(),
            topology :: ets:tid(),
            policy   :: fun((method()) -> method() | false),
            listener :: pid(),
            backend  :: pid(),
            frontend :: pid(),
            client   :: inet:socket(),
            server   :: inet:socket()}).

-define(TIMEOUT, 1000).

%%
%% API
%%

-spec start_link(pid(), client(), cowboy_tcp_transport, frontend())
                -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(Listener, Client, cowboy_tcp_transport, Config) ->
    gen_fsm:start_link(?MODULE, {Listener, Client, Config}, []).

-spec reply(pid(), iolist()) -> ok.
%% @doc
reply(Conn, Raw) ->
    gen_fsm:send_event(Conn, {reply, Raw}).

-spec reply(pid(), pos_integer(), method(), protocol()) -> ok.
%% @doc
reply(Conn, Channel, Method, Protocol) ->
    gen_fsm:send_event(Conn, {reply, Channel, Method, Protocol}).

-spec replay(pid(), #'connection.start_ok'{}, iolist(), protocol()) -> ok.
%% @doc
replay(Conn, StartOk, Replay, Protocol) ->
    gen_fsm:sync_send_event(Conn, {replay, StartOk, Replay, Protocol}).

-spec forward(pid(), iolist(), pos_integer(), method(), protocol()) -> ok.
%% @doc
forward(Conn, Raw, Channel, Method, Protocol) ->
    gen_fsm:send_event(Conn, {forward, Raw, Channel, Method, Protocol}).

%%
%% Callbacks
%%

-spec init(_) -> {ok, idle, #s{}}.
%% @hidden
init({Listener, Client, Config}) ->
    process_flag(trap_exit, true),
    totochtin_stats:connect(self()),
    {ok, accepting, #s{router   = totochtin_router:new(Config),
                       listener = Listener,
                       client   = Client}}.

handshaking({reply, Channel, Method, Protocol}, State = #s{client = Client}) ->
    ok = send(Client, Channel, Method, Protocol),
    {next_state, handshaking, State}.

handshaking({replay, StartOk, Replay, Protocol}, _From,
          State = #s{router = Router, topology = Topology}) ->
    %% Get a server address according to the routing and relevant balancer
    {Name, Addr, Policies} = totochtin_router:route(Router, StartOk, Protocol),
    %% Start a backend, replaying the previous client data to the server
    {ok, Pid, Sock} = totochtin_backend:start_link(self(), Addr, Replay),
    %% Create a fun composed of all available policies
    Handler = totochtin_policy:handler(Name, Topology, Protocol, Policies),
    {reply, ok, proxying, State#s{backend = Pid,
                                  server  = Sock,
                                  policy  = Handler}}.

proxying({reply, Raw}, State = #s{client = Client}) ->
    ok = send(Client, Raw),
    {next_state, proxying, State};
proxying({forward, Raw, Channel, Method, Protocol}, State) ->
    ok = intercept(Raw, Channel, Method, Protocol, State),
    {next_state, proxying, State}.

closing(timeout, State) ->
    lager:notice("FSM-CLOSING-TIMEOUT"),
    {stop, normal, State};
closing({reply, Raw}, State = #s{client = Client}) ->
    lager:notice("FSM-SERVER-CLOSE-OK"),
    ok = send(Client, Raw),
    {stop, normal, State};
closing({forward, _Raw, _Channel, Method = #'connection.close_ok'{}, Protocol},
        State = #s{server = Server}) ->
    lager:info("FSM-CLIENT-CLOSE-OK"),
    ok = send(Server, 0, Method, Protocol),
    {stop, normal, State};
closing({forward, _Raw, _Channel, _Method, _Protocol}, State) ->
    {next_state, closing, State, ?TIMEOUT}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%% @hidden
handle_info({'EXIT', Pid, _Msg}, _Any, State = #s{frontend = Pid}) ->
    ok = close_server(State),
    lager:info("FRONTEND-EXIT ~p", [Pid]),
    {next_state, closing, State, ?TIMEOUT};
handle_info({'EXIT', Pid, _Msg}, _Any, State = #s{backend = Pid}) ->
    ok = close_client(State),
    lager:info("BACKEND-EXIT ~p", [Pid]),
    {next_state, closing, State, ?TIMEOUT};

%% Cowboy acknowledgement
handle_info({shoot, Listener}, accepting,
            State = #s{listener = Listener, client = Client}) ->
    %% Cowboy acknowledgement
    lager:info("FSM-ACCEPT"),
    {ok, Frontend} = totochtin_frontend:start_link(self(), Client),
    {next_state, handshaking, State#s{frontend = Frontend}}.

%% @hidden
terminate(Reason, _StateName, State = #s{server   = Server,
                                         backend  = Backend,
                                         client   = Client,
                                         frontend = Frontend}) ->
    lager:error("FSM-EXIT ~p", [Reason]),
    %% Investigate sockets release before send/2 messages are flushed
    catch gen_tcp:close(Server),
    catch gen_tcp:close(Client),
    catch exit(Backend, kill),
    catch exit(Frontend, kill),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Private
%%

-spec send(inet:socket(), binary()) -> ok | {error, _}.
%% @private
send(Sock, Data) ->
    case gen_tcp:send(Sock, Data) of
        ok    -> ok;
        Error -> lager:debug("CONN-CLOSED ~p - ~p", [Error, Data])
    end.

-spec send(inet:socket(), non_neg_integer(), method(), protocol()) -> ok.
%% @private
send(Sock, 0, Method, Protocol) ->
    rabbit_writer:internal_send_command(Sock, 0, Method, Protocol);
send(Sock, Channel, Method, Protocol) ->
    lager:notice("~p, ~p, ~p, ~p", [Sock, Channel, Method, Protocol]),
    Frame =
        rabbit_binary_generator:build_simple_method_frame(Channel, Method, Protocol),
    send(Sock, Frame).

-spec intercept(iolist(), non_neg_integer(), ignore | passthrough | method(),
                protocol(), #s{}) -> ok.
%% @private
intercept(_Data, _Channel, ignore, _Protocol, _State) ->
    ok;
intercept(Data, _Channel, passthrough, _Protocol, #s{server = Server}) ->
    send(Server, Data);
intercept(Data, Channel, Method, Protocol, #s{server = Server, policy = Policy}) ->
    case Policy(Method) of
        false     -> send(Server, Data);
        NewMethod -> send(Server, Channel, NewMethod, Protocol)
    end.

-spec close_client(#s{}) -> ok.
%% @private
close_client(#s{frontend = Frontend, client = Client}) ->
    close(Client).

-spec close_server(#s{}) -> ok.
%% @private
close_server(#s{backend = undefined}) ->
    ok;
close_server(#s{backend = Backend, server = undefined}) ->
    exit(Backend, kill),
    ok;
close_server(#s{backend = Backend, server = Server}) ->
    close(Server).

close(Sock) ->
    Close = #'connection.close'{reply_text = <<"Goodbye">>,
                                reply_code = 200,
                                class_id   = 0,
                                method_id  = 0},
    send(Sock, 0, Close, rabbit_framing_amqp_0_9_1).
