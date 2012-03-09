%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_sup).

-behaviour(supervisor).

-include("include/totochtin.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

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
    Topology = topology_spec(),
    Stats = stats_spec(totochtin:config(statsd)),
    Balancers = [balancer_spec(B) || B <- totochtin:config(backends)],
    {ok, {{one_for_one, 3, 20}, [Topology, Stats|Balancers]}}.

%%
%% Topology Map
%%

topology_spec() ->
    {topology, {totochtin_topology, start_link, []},
     permanent, 2000, worker, [totochtin_topology]}.

%%
%% Grpoc, Graphite
%%

stats_spec(Config) ->
    Ns = totochtin:option(namespace, Config),
    Url = totochtin:os_env(totochtin:option(url, Config), "localhost:8126"),
    {stats, {totochtin_stats, start_link, [Ns, Url]},
     permanent, 2000, worker, [totochtin_stats]}.

%%
%% Balancers
%%

balancer_spec({Name, Config}) ->
    Mod = totochtin:option(balancer, Config),
    Args = [Name,
            Mod,
            node_addresses(Config),
            totochtin:option(policies, Config)],
    {Name, {totochtin_balancer, start_link, Args},
     permanent, 2000, worker, [totochtin_balancer]}.

node_addresses(Config) -> [address(Addr) || Addr <- totochtin:option(nodes, Config)].

address(Addr) -> {totochtin:option(ip, Addr), totochtin:option(port, Addr)}.
