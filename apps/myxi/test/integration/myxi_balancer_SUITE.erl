-module(myxi_balancer_SUITE).

-include("myxi_common_test.hrl").

-define(BALANCER, myxi_dummy_balancer).

%% Behaviour under test
-behaviour(myxi_balancer).
-export([next/1]).

%%
%% Callbacks
%%

init_per_testcase(_Case, Config) ->
    %% Mock net_adm:ping/1 for health_checks
    meck:new(net_adm, [passthrough, unstick]),
    %% Mock the topology
    meck:new(myxi_topology, [passthrough]),
    meck:expect(myxi_topology, add_endpoints, 1, ok),
    %% Start the balancer with this module as the callback
    {ok, Balancer} = myxi_balancer:start_link(?BALANCER, ?MODULE, [], [], 0),
    [{mocks, [net_adm, myxi_topology]}, {balancer, Balancer}|Config].

end_per_testcase(_Case, Config) ->
    meck:unload(?config(mocks, Config)),
    myxi_balancer:stop(?config(balancer, Config)),
    clear([mocks, balancer], Config).

all() -> [add_endpoints, register_balancer].

%%
%% Tests
%%

add_endpoints(Config) ->
    Pid = ?config(balancer, Config),
    ?assert(meck:called(myxi_topology, add_endpoints, [], Pid)).

register_balancer(_Config) ->
    ?assert(is_pid(whereis(?BALANCER))).

%%
%% Behaviour
%%

next([H|T]) -> {H, T ++ [H]}.

%%
%% Helpers
%%

balancer(Config) -> ?config(pid, Config).

clear([], Config)    -> Config;
clear([H|T], Config) -> clear(T, lists:keydelete(H, 1, Config)).
