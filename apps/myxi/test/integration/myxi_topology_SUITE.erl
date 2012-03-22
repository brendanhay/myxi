%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_topology_SUITE).

-include("/include/myxi_common_test.hrl").

-define(BALANCER, myxi_dummy_balancer).
-define(UP,       check_up).
-define(DOWN,     check_down).

%%
%% Callbacks
%%

init_per_testcase(_Case, Config) ->
    meck:new(rpc, [unstick]),
    myxi_topology:start_link(),
    [{mocks, [rpc]}|Config].

end_per_testcase(_Case, Config) ->
    meck:unload(?config(mocks, Config)),
    myxi_topology:stop(),
    suite_helper:clear([mocks], Config).

all() -> [register_topology, initial_state_empty, find_added_exchanges].

%%
%% Tests
%%

register_topology(_Config) ->
    ?assert(is_pid(whereis(myxi_topology))).

initial_state_empty(_Config) ->
    ?assertEqual([], myxi_topology:find_exchange('_')).

find_added_exchanges(_Config) ->
    %% Create some endpoints
    E1 = #endpoint{node = N1 = nonsense, backend = B1 = boner},
    E2 = #endpoint{node = N2 = raging, backend = B2 = something},

    %% Create some exchanges
    EX1 = exchange(EN1 = <<"mamut">>),
    EX2 = exchange(EN2 = <<"grayson">>),

    %% Ensure the rpc:call returns some exchanges
    meck:expect(rpc, call,
                fun
                    (N, _, _, _) when N =:= N1 -> [EX1];
                    (_, _, _, _)               -> [EX2]
                end),

    %% Add the endpoints/exchanges to the topology
    myxi_topology:add_endpoints([E1, E2]),

    ?assertMatch([{B1, #'exchange.declare'{}}], myxi_topology:find_exchange(EN1)),
    ?assertMatch([{B2, #'exchange.declare'{}}], myxi_topology:find_exchange(EN2)).


%%
%% Helpers
%%

exchange(Name) -> #exchange{name = #resource{name = Name}}.
