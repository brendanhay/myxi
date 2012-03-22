%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_balancer_SUITE).

-include("/include/myxi_common_test.hrl").

-define(BALANCER, myxi_dummy_balancer).
-define(UP,       check_up).
-define(DOWN,     check_down).

%%
%% Callbacks
%%

init_per_testcase(_Case, Config) ->
    %% Mock net_adm:ping/1 for health_checks
    meck:new(net_adm, [unstick]),
    meck:expect(net_adm, ping, 1, pong),

    %% Mock the topology
    meck:new(myxi_topology),
    meck:expect(myxi_topology, add_endpoints, 1, ok),

    %% Start the balancer with this module as the callback
    Endpoints = [#endpoint{node = A, backend = A, address = {A, 1}} ||
                    A <- [a, b, c]],
    {ok, Pid} = myxi_balancer:start_link(?BALANCER, myxi_roundrobin_balancer,
                                         Endpoints, [], 5000),

    [{mocks, [net_adm, myxi_topology]},
     {endpoints, Endpoints},
     {balancer, Pid}|Config].

end_per_testcase(_Case, Config) ->
    meck:unload(?config(mocks, Config)),
    myxi_balancer:stop(?config(balancer, Config)),
    suite_helper:clear([mocks, balancer], Config).

all() -> [add_endpoints,
          register_balancer,
          all_endpoints_up,
          all_endpoints_down,
          single_endpoint_down].

%%
%% Tests
%%

add_endpoints(Config) ->
    Pid = ?config(balancer, Config),
    Args = ?config(endpoints, Config),
    ?assert(meck:called(myxi_topology, add_endpoints, [Args], Pid)).

register_balancer(_Config) ->
    ?assert(is_pid(whereis(?BALANCER))).

all_endpoints_up(Config) ->
    [?assertEqual({ok, {A, []}}, myxi_balancer:next(?BALANCER)) ||
        A <- ?config(endpoints, Config)].

all_endpoints_down(Config) ->
    meck:expect(net_adm, ping, 1, pang),
    suite_helper:send(?BALANCER, ?UP),

    [?assertEqual({error, down}, myxi_balancer:next(?BALANCER)) ||
        _A <- ?config(endpoints, Config)].

single_endpoint_down(Config) ->
    All = ?config(endpoints, Config),
    Down = #endpoint{node = Node} = lists:nth(2, All),
    Up = All -- [Down],

    %% Mark one node as down
    meck:expect(net_adm, ping, fun(N) when N =:= Node -> pang; (_) -> pong end),

    %% Force a check
    suite_helper:send(?BALANCER, ?UP),

    %% Assert the expected up nodes are returned
    [?assertEqual({ok, {A, []}}, myxi_balancer:next(?BALANCER)) || A <- Up],

    %% Mark all nodes as now up
    meck:expect(net_adm, ping, 1, pong),

    %% Force a check of the down node
    suite_helper:send(?BALANCER, ?DOWN),

    %% Assert everything is now returned
    Actual = [E || {ok, {E, []}} <- [myxi_balancer:next(?BALANCER) || _A <- All]],
    ?assertEqual(lists:usort(All), lists:usort(Actual)).
