%% @doc
-module(poxy_backend).

%% API
-export([start_link/3]).

%% Callbacks
-export([init/4]).

-include("include/poxy.hrl").

-type state() :: {client(), server()}.

%%
%% API
%%

-spec start_link(addr(), client(), replay()) -> {ok, pid(), server()}.
%% @doc
start_link(Addr, Client, Replay) ->
    proc_lib:start_link(?MODULE, init, [self(), Addr, Client, Replay]).

%%
%% Callbacks
%%

-spec init(pid(), addr(), client(), replay()) -> no_return().
init(Frontend, Addr, Client, Replay) ->
    {Ip, Port} = {poxy:option(ip, Addr), poxy:option(port, Addr)},
    Server = connect(Ip, Port),
    log("CONN", State = {Client, Server}),
    ok = replay(Server, Replay),
    log("REPLAY", State),
    proc_lib:init_ack(Frontend, {ok, self(), Server}),
    loop(State).

%%
%% Private
%%

-spec connect(inet:ip_address(), inet:port_number()) -> server().
%% @private
connect(Ip, Port) ->
    Tcp = [binary, {active, false}, {packet, raw}],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Socket} ->
            Socket;
        Error ->
            lager:error("BACKEND-ERR", Error),
            throw({backend_unavailable, Ip, Port, Error})
    end.

-spec replay(server(), replay()) -> ok.
%% @private
replay(Server, [Payload, Header, Handshake]) ->
    ok = gen_tcp:send(Server, Handshake),
    ok = case gen_tcp:recv(Server, 0) of
             {ok, _Data} -> ok
         end,
    gen_tcp:send(Server, [Header, Payload]).

-spec loop(state()) -> no_return().
%% @private
loop(State = {Client, Server}) ->
    ok = case gen_tcp:recv(Server, 0) of
             {ok, Data} ->
                 gen_tcp:send(Client, Data);
             {error, closed} ->
                 log("CLOSED", State),
                 exit(normal);
             Error ->
                 lager:error("BACKEND-ERR", [Error]),
                 throw({backend_error, Error})
         end,
    loop(State).

%%
%% Logging
%%

-spec log(string() | atom(), state()) -> ok.
%% @private
log(Mode, {Client, Server}) ->
    lager:info("BACKEND-~s ~s -> ~p -> ~s",
               [Mode, poxy:peername(Server), self(), poxy:peername(Client)]).
