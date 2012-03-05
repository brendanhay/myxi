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

balancer_spec({Name, Opts}) ->
    Mod = poxy:option(balance, Opts),
    {Name, {poxy_balancer, start_link, [Name, Mod, poxy:option(nodes, Opts)]},
     permanent, 2000, worker, [poxy_balancer]}.
