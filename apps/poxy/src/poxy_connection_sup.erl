%% @doc
-module(poxy_connection_sup).
-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Callbacks
-export([init/1]).

-include("include/poxy.hrl").

%%
%% API
%%

-spec start_link(pid(), client(), cowboy_tcp_transport, frontend())
                -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(Listener, Client, cowboy_tcp_transport, Config) ->
    ok = inet:setopts(Client, [{active, false}]),
    case supervisor:start_link(?MODULE, []) of
        {ok, Pid} ->
            Writer = start_writer(Pid, Client),
            lager:info("WRITER ~p", [Writer]),
            Frontend = start_frontend(Pid, Writer, Client, Config),
            lager:info("FRONTEND ~p", [Frontend]),
            {cowboy:accept_ack(Listener), Pid};
        Error ->
            lager:error("SUP-ERR", [Error]),
            throw(Error)
    end.

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 0, 1}, []}}.
%% @hidden
init([]) -> {ok, {{one_for_all, 0, 1}, []}}.

%%
%% Private
%%

%% @private
start_child(Sup, Spec) ->
    {ok, Pid} = supervisor:start_child(Sup, Spec),
    Pid.

%%
%% Staged Startup
%%

%% @private
start_backend(Sup, Writer, Addr, Replay, Inters) ->
    Spec = {backend, {poxy_backend, start_link, [Writer, Addr, Replay, Inters]},
            permanent, 2000, worker, [poxy_backend]},
    start_child(Sup, Spec).

%% @private
start_frontend(Sup, Writer, Client, Config) ->
    BackendStart =
        fun(Addr, Replay, Inters) ->
                start_backend(Sup, Writer, Addr, Replay, Inters)
        end,
    Spec = {frontend, {poxy_frontend, start_link,
                       [Writer, Client, BackendStart, Config]},
            permanent, 2000, worker, [poxy_frontend]},
    start_child(Sup, Spec).

