%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_federation_middleware_tests).

-include_lib("myxi_lib/include/myxi_test.hrl").

-define(NODE,    node()).
-define(BACKEND, myxi).

%%
%% Fixtures
%%

setup() ->
    meck:new(Mods = [myxi_topology, rabbit_federation_upstream]),
    meck:expect(rabbit_federation_upstream, from_set, 2, {ok, great_success}),
    Mods.

teardown(Mods) ->
    meck:unload(Mods).

federation_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      {"Add a pre function callback", fun add_federation_callback/0}
     ]}.

%%
%% Units
%%

add_federation_callback() ->
    %% Return a non-existing backend for any find_exchange/1 call
    meck:expect(myxi_topology, find_exchange, 1,
                [{left_field, #'exchange.declare'{}}]),

    %% Run the middleware
    Endpoint = #endpoint{node = ?NODE, backend = ?BACKEND},
    Method = #'queue.bind'{exchange = <<"ball.sacks">>},
    Before = #mware{endpoint = Endpoint, method = Method},
    After = #mware{pre = Pre} = myxi_federation_middleware:call(Before),

    %% Assert the queue.bind wasn't modified
    ?assertEqual(Before#mware.method, After#mware.method),

    %% Assert that only one pre callback was added
    ?assertEqual(1, length(Pre)),

    %% Check the callback is contains the expected arguments
    {fn, Fn} = hd(Pre),

    %% Assert the callback is a function
    ?assert(is_function(Fn)).

%%
%% Properties
%%

other_amqp_methods_unmodified_test() ->
    ?EQC(?FORALL(M, myxi_generators:amqp_methods_except(#'queue.bind'{}),
                 begin
                     Expected = #mware{method = M},
                     Expected =:= myxi_federation_middleware:call(Expected)
                 end)).
