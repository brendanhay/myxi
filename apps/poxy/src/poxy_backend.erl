%% @doc
-module(poxy_backend).

%% API
-export([load/1,
         start_link/3]).

%% Callbacks
-export([init/4]).

-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include("include/poxy.hrl").

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
    Server = match({login, User}),
    ok = replay(Server, Replay),
    proc_lib:init_ack(Frontend, {ok, self(), Server}),
    loop(Client, Server).

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
    lager:info("BACKEND-CONN"),
    Tcp = [binary, {active, false}, {packet, raw}],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Socket} -> Socket;
        Error        -> error({backend_unavailable, Ip, Port})
    end.

-spec replay(server(), replay()) -> ok.
%% @private
replay(Server, [Payload, Header, Handshake]) ->
    lager:info("BACKEND-REPLAY"),
    ok = gen_tcp:send(Server, Handshake),
    ok = case gen_tcp:recv(Server, 0) of
             {ok, _Data} -> ok
         end,
    gen_tcp:send(Server, [Header, Payload]).

-spec loop(client(), server()) -> no_return().
%% @private
loop(Client, Server) ->
    ok = case gen_tcp:recv(Server, 0) of
             {ok, Data} ->
                 lager:info("BACKEND-RECV"),
                 gen_tcp:send(Client, Data);
             {error, closed} ->
                 exit(normal);
             Error ->
                 exit({backend_error, Error})
         end,
    loop(Client, Server).
