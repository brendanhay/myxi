%% @doc
-module(poxy_backend).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/5]).

-include("include/poxy.hrl").

-record(s, {writer :: pid(),
            server :: inet:socket()}).

%%
%% API
%%

-spec start_link(pid(), addr(), replay(), intercepts()) -> {ok, pid()}.
%% @doc
start_link(Writer, Addr, Replay, Inters) ->
    proc_lib:start_link(?MODULE, init, [self(), Writer, Addr, Replay, Inters]).

%%
%% Callbacks
%%

-spec init(pid(), pid(), addr(), replay(), intercepts()) -> no_return().
init(Sup, Writer, Addr, Replay, Inters) ->
    {Ip, Port} = {poxy:option(ip, Addr), poxy:option(port, Addr)},
    Server = connect(Ip, Port),
    State = #s{writer = Writer, server = Server},
    log("CONN", State),
    proc_lib:init_ack(Sup, {ok, self()}),

    %% Send server socket to writer with replay and intercepts
    poxy_backend_writer:replay(Writer, Server, Replay, Inters),

    loop(State).

%%
%% Private
%%

-spec connect(inet:ip_address(), inet:port_number()) -> server().
%% @private
connect(Ip, Port) ->
    Tcp = [binary, {active, false}, {packet, raw}|poxy:config(tcp)],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Socket} ->
            Socket;
        Error ->
            lager:error("BACKEND-ERR", Error),
            throw({backend_unavailable, Ip, Port, Error})
    end.

-spec loop(#s{}) -> no_return().
%% @private
loop(State = #s{writer = Writer, server = Server}) ->
    ok = case gen_tcp:recv(Server, 0) of
             {ok, Data} ->
                 poxy_writer:reply(Writer, Data);
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

-spec log(string() | atom(), #s{}) -> ok.
%% @private
log(Mode, #s{server = Server}) ->
    lager:info("BACKEND-~s ~s -> ~p", [Mode, poxy:peername(Server), self()]).
