-module(myxi_balancer_SUITE).

-include("myxi_common_test.hrl").

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
    clear([mocks, balancer], Config).

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
    ?BALANCER ! ?UP,
    context_switch(),
    [?assertEqual({error, down}, myxi_balancer:next(?BALANCER)) ||
        _A <- ?config(endpoints, Config)].

single_endpoint_down(Config) ->
    All = ?config(endpoints, Config),
    Down = #endpoint{node = Node} = lists:nth(2, All),
    Up = All -- [Down],

    meck:expect(net_adm, ping, fun(N) when N =:= Node -> pang; (_) -> pong end),

    ?BALANCER ! ?UP,
    context_switch(),

    [?assertEqual({ok, {A, []}}, myxi_balancer:next(?BALANCER)) || A <- Up],

    meck:expect(net_adm, ping, 1, pong),

    ?BALANCER ! ?DOWN,
    context_switch(),

    %% Check they're all up
    Actual = [E || {ok, {E, []}} <- [myxi_balancer:next(?BALANCER) || _A <- All]],
    ?assertEqual(lists:usort(All), lists:usort(Actual)).

%%
%% Helpers
%%

clear([], Config)    -> Config;
clear([H|T], Config) -> clear(T, lists:keydelete(H, 1, Config)).

context_switch() -> erlang:bump_reductions(2000).

