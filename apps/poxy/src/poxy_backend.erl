%% @doc
-module(poxy_backend).

%% API
-export([load/1,
         start_link/3]).

%% Callbacks
-export([init/4]).

-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include("include/poxy.hrl").

-type state() :: {client(), server()}.

%%
%% API
%%

-spec load([backend()]) -> ok.
%% @doc
load(Backends) ->
    [backend(Opts) || Opts <- Backends],
    ok.

-spec start_link(client(), user(), replay()) -> {ok, pid(), server()}.
%% @doc
start_link(Client, User, Replay) ->
    proc_lib:start_link(?MODULE, init, [self(), Client, User, Replay]).

%%
%% Callbacks
%%

-spec init(pid(), client(), user(), replay()) -> no_return().
init(Frontend, Client, User, Replay) ->
    State = {Client, Server = match({login, User})},
    log("CONN", State),
    ok = replay(Server, Replay),
    log("REPLAY", State),
    proc_lib:init_ack(Frontend, {ok, self(), Server}),
    loop(State).

%%
%% Private
%%

-spec match(match()) -> server().
%% @private
match({login, Login}) when is_binary(Login) ->
    Mod = list_to_atom(binary_to_list(Login)),
    case mochiglobal:get(Mod) of
        {Ip, Port} -> connect(Ip, Port);
        undefined  -> error({backend_notfound, Mod})
    end;
match(_Match) ->
    error(match_not_supported).

-spec backend(options()) -> ok.
%% @private
backend(Opts) ->
    mochiglobal:put(poxy:option(match, Opts),
                    {poxy:option(ip, Opts), poxy:option(port, Opts)}).

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
                 log("RECV", State),
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
