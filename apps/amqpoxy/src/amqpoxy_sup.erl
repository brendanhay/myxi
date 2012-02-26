%% @doc
-module(amqpoxy_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1]).

-include("include/amqpoxy.hrl").

%%
%% API
%%

-spec start_link([backend()]) -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(Backends) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Backends).

%%
%% Callbacks
%%

-spec init([backend()]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init(Backends) ->
    ok = amqpoxy_router:load(Backends),
    {ok, {{one_for_all, 3, 20}, []}}.
