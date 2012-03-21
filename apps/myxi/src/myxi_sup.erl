%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_sup).

-behaviour(supervisor).

-include("include/myxi.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(BALANCER_DELAY, 8000).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    %% Used to ensure balancer check starts are delayed
    random:seed(erlang:now()),
    Topology = topology_spec(),
    Stats = stats_spec(myxi_util:config(statsd)),
    Balancers = [balancer_spec(B) || B <- myxi_util:config(backends)],
    {ok, {{one_for_one, 3, 20}, [Topology, Stats|Balancers]}}.

%%
%% Topology Map
%%

topology_spec() ->
    {topology, {myxi_topology, start_link, []},
     permanent, 2000, worker, [myxi_topology]}.

%%
%% Grpoc, Graphite
%%

stats_spec(Config) ->
    Ns = myxi_util:option(namespace, Config),
    Url = myxi_util:os_env(myxi_util:option(url, Config), "localhost:8126"),
    {stats, {myxi_stats, start_link, [Ns, Url]},
     permanent, 2000, worker, [myxi_stats]}.

%%
%% Balancers
%%

balancer_spec({Name, Config}) ->
    Mod = myxi_util:option(balancer, Config),
    Args = [Name,
            Mod,
            endpoints(Name, Config),
            myxi_util:option(middleware, Config),
            random:uniform(?BALANCER_DELAY)],
    {Name, {myxi_balancer, start_link, Args},
     permanent, 2000, worker, [myxi_balancer]}.

endpoints(Name, Config) ->
    [endpoint(Name, N) || N <- myxi_util:option(nodes, Config)].

endpoint(Name, Options) ->
    Node = myxi_util:option(node, Options),
    Addr = {myxi_util:hostname(Node), myxi_util:option(port, Options)},
    #endpoint{node    = Node,
              backend = Name,
              address = Addr}.
