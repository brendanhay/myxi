%% @doc
-module(poxy_backend).

%% API
-export([start_link/2,
         connect/3]).

%% Callbacks
-export([init/3]).

-include("include/poxy.hrl").

-record(s, {frontend :: pid(),
            client   :: inet:socket(),
            server   :: inet:socket()}).

%%
%% API
%%

-spec start_link(pid(), inet:socket()) -> {ok, pid()}.
%% @doc
start_link(Frontend, Client) ->
    proc_lib:start_link(?MODULE, init, [self(), Frontend, Client]).

%% @doc
connect(Backend, Addr, Replay) ->
    Backend ! {connect, self(), Addr, Replay},
    receive
        {connected, Server} -> {ok, Server}
    after
        2000                -> timeout
    end.

%%
%% Callbacks
%%

-spec init(pid(), pid(), inet:socket()) -> no_return().
%% @hidden
init(Parent, Frontend, Client) ->
    lager:info("BACKEND-INIT ~p", [self()]),
    proc_lib:init_ack(Parent, {ok, self()}),
    idle(#s{frontend = Frontend, client = Client}).

%%
%% States
%%

%% @private
idle(State) ->
    receive
        {connect, From, Addr, Replay} ->
            {Ip, Port} = {poxy:option(ip, Addr), poxy:option(port, Addr)},
            From ! {connected, Server = connect(Ip, Port, State, 3)},
            ok = poxy_writer:replay(Server, Replay),
            read(State#s{server = Server});
        Msg ->
            disconnect({backend_unexpected_message, Msg}, State)
    end.

-spec read(#s{}) -> no_return().
%% @private
read(State = #s{server = Server, client = Client}) ->
    case gen_tcp:recv(Server, 0) of
        {ok, Data} ->
            poxy_writer:send(Client, Data),
            read(State);
        {error, closed} ->
            disconnect(server_closed, State);
        Error ->
            disconnect(Error, State)
    end.

%%
%% Private
%%

-spec connect(addr(), #s{}, non_neg_integer()) -> inet:socket().
%% @private
connect(Ip, Port, State, 0) ->
    disconnect({backend_timeout, Ip, Port}, State);
connect(Ip, Port, State, Retries) ->
    Tcp = [binary, {active, false}, {packet, raw}|poxy:config(tcp)],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Server} ->
            Server;
        Error ->
            lager:error("BACKEND-ERR ~p", [{Error, Ip, Port}]),
            timer:sleep(500),
            connect(Ip, Port, State, Retries - 1)
    end.

-spec disconnect(any(), #s{}) -> no_return().
%% @private
disconnect(Error, #s{frontend = Frontend, server = Server, client = Client}) ->
    lager:error("BACKEND-ERR ~p", [Error]),
    poxy:disconnect([Server, Client]),
    exit(Frontend, backend_disconnect),
    exit(normal).
