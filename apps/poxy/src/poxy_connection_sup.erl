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
    case supervisor:start_link(?MODULE, []) of
        {ok, Pid} ->
            staged_startup(Pid, Listener, Client, Config),
            {ok, Pid};
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
%% Startup
%%

staged_startup(Sup, Listener, Client, Config) ->
    Backend = start_backend(Sup, Client),
    _Frontend = start_frontend(Sup, Backend, Client, Config),
    cowboy:accept_ack(Listener).

%% @private
start_backend(Sup, Client) ->
    Spec = {backend, {poxy_backend, start_link, [Client]},
            permanent, brutal_kill, worker, [poxy_backend]},
    start_child(Sup, Spec).

%% @private
start_frontend(Sup, Backend, Client, Config) ->
    Spec = {frontend, {poxy_frontend, start_link, [Backend, Client, Config]},
            permanent, brutal_kill, worker, [poxy_frontend]},
    start_child(Sup, Spec).

%% @private
start_child(Sup, Spec) ->
    {ok, Pid} = supervisor:start_child(Sup, Spec),
    Pid.

