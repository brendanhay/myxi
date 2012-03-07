%% @doc
-module(poxy_backend).

%% API
-export([start_link/1,
         connect/3]).

%% Callbacks
-export([init/2]).

-include("include/poxy.hrl").

-record(s, {client :: inet:socket(),
            server :: inet:socket()}).

%%
%% API
%%

-spec start_link(pid()) -> {ok, pid()}.
%% @doc
start_link(Client) -> proc_lib:start_link(?MODULE, init, [self(), Client]).

%% @doc
connect(Backend, Addr, Replay) ->
    Backend ! {connect, self(), Addr, Replay},
    receive
        {connected, Server} -> {ok, Server}
    after
        2000                -> throw(backend_timeout)
    end.

%%
%% Callbacks
%%

-spec init(pid(), inet:socket()) -> no_return().
%% @hidden
init(Sup, Client) ->
    lager:info("BACKEND-INIT"),
    proc_lib:init_ack(Sup, {ok, self()}),
    idle(#s{client = Client}).

%%
%% States
%%

%% @private
idle(State) ->
    receive
        {connect, From, Addr, Replay} ->
            From ! {connected, Server = connect(Addr)},
            ok = poxy_writer:replay(Server, Replay),
            connected(State#s{server = Server});
        Msg ->
            lager:error("BACKEND-ERR", [{unexpected_message, Msg}])
    end.

-spec connected(#s{}) -> no_return().
%% @private
connected(State = #s{client = Client, server = Server}) ->
    ok = case gen_tcp:recv(Server, 0) of
             {ok, Data} ->
                 poxy_writer:send(Client, Data);
             {error, closed} ->
                 log("CLOSED", State),
                 exit(normal);
             Error ->
                 lager:error("BACKEND-ERR", [Error]),
                 throw({backend_error, Error})
         end,
    connected(State).

%%
%% Private
%%

-spec connect(addr()) -> server().
%% @private
connect(Addr) ->
    Tcp = [binary, {active, false}, {packet, raw}|poxy:config(tcp)],
    {Ip, Port} = {poxy:option(ip, Addr), poxy:option(port, Addr)},
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Server} ->
            Server;
        Error ->
            lager:error("BACKEND-ERR", Error),
            throw({backend_unavailable, Ip, Port, Error})
    end.

-spec log(string() | atom(), #s{}) -> ok.
%% @private
log(Mode, #s{server = Server}) ->
    lager:info("BACKEND-~s ~s -> ~p", [Mode, poxy:peername(Server), self()]).
