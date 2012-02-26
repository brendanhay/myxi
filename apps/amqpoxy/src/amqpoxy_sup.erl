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

-spec start_link([backend()]) -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link(Backends) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Backends).

%%
%% Callbacks
%%

%% @hidden
init(Backends) ->
    Balancer = {amqpoxy_balancer,
                {amqpoxy_balancer, start_link, [Backends]},
                permanent, 6000, worker, [amqpoxy_balancer]},
    {ok, {{one_for_all, 3, 20}, [Balancer]}}.
