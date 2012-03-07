%% @doc
-module(poxy_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-include("include/poxy.hrl").

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    {ok, {{one_for_one, 3, 20},
          [balancer_spec(B) || B <- poxy:config(backends)]}}.

%%
%% Private
%%

balancer_spec({Name, Config}) ->
    Mod = poxy:option(balancer, Config),
    Args = [Name,
            Mod,
            poxy:option(nodes, Config),
            poxy:option(interceptors, Config)],
    {Name, {poxy_balancer, start_link, Args},
     permanent, 2000, worker, [poxy_balancer]}.
