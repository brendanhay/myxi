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
                    A <- [c, b, a]],
    {ok, Balancer} = myxi_balancer:start_link(?BALANCER, myxi_roundrobin_balancer,
                                              Endpoints, [], 5000),

    [{mocks, [net_adm, myxi_topology]},
     {endpoints, Endpoints},
     {balancer, Balancer}|Config].

end_per_testcase(_Case, Config) ->
    meck:unload(?config(mocks, Config)),
    myxi_balancer:stop(?config(balancer, Config)),
    clear([mocks, balancer], Config).

all() -> [add_endpoints,
          register_balancer,
          endpoints_up,
          endpoints_down].

%%
%% Tests
%%

add_endpoints(Config) ->
    Pid = ?config(balancer, Config),
    Args = ?config(endpoints, Config),
    ?assert(meck:called(myxi_topology, add_endpoints, [Args], Pid)).

register_balancer(_Config) ->
    ?assert(is_pid(whereis(?BALANCER))).

endpoints_up(Config) ->
    [?assertEqual({ok, {A, []}}, myxi_balancer:next(?BALANCER)) ||
        A <- ?config(endpoints, Config)].

endpoints_down(Config) ->
    meck:expect(net_adm, ping, 1, pang),
    ?BALANCER ! ?UP,
    erlang:bump_reductions(2000),
    [?assertEqual({error, down}, myxi_balancer:next(?BALANCER)) ||
        _A <- ?config(endpoints, Config)].

%%
%% Helpers
%%

balancer(Config) -> ?config(pid, Config).

clear([], Config)    -> Config;
clear([H|T], Config) -> clear(T, lists:keydelete(H, 1, Config)).

