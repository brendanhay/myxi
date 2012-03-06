%% @doc
-module(poxy_backend).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/5]).

-include("include/poxy.hrl").

-record(s, {client :: pid(),
            server :: pid(),
            sock   :: inet:socket()}).

%%
%% API
%%

-spec start_link(pid(), addr(), replay(), intercepts()) -> {ok, pid()}.
%% @doc
start_link(Client, Addr, Replay, Inters) ->
    proc_lib:start_link(?MODULE, init, [self(), Client, Addr, Replay, Inters]).

%%
%% Callbacks
%%

-spec init(pid(), pid(), addr(), replay(), intercepts()) -> no_return().
init(Sup, Client, Addr, Replay, Inters) ->
    {Ip, Port} = {poxy:option(ip, Addr), poxy:option(port, Addr)},

    Sock = connect(Ip, Port),

    {ok, Server} = poxy_backend_writer:start_link(Sock, Replay, Inters),

    State = #s{server = Server, client = Client, sock = Sock},

    log("CONN", State),

    proc_lib:init_ack(Sup, {ok, Server}),
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
loop(State = #s{client = Client, sock = Sock}) ->
    ok = case gen_tcp:recv(Sock, 0) of
             {ok, Data} ->
                 poxy_frontend_writer:reply(Client, Data);
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
log(Mode, #s{sock = Sock}) ->
    lager:info("BACKEND-~s ~s -> ~p", [Mode, poxy:peername(Sock), self()]).
