%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_topology_middleware_tests).

-include_lib("myxi/include/myxi_test.hrl").

%%
%% Units
%%

add_topology_verify_callback_test() ->
    Exchange = <<"ten.ton.whale">>,
    Backend = rumplestiltskin,

    %% Call the middleware
    Before = #mware{method   = #'exchange.declare'{exchange = Exchange},
                    endpoint = #endpoint{backend = Backend}},
    After = #mware{post = Post} = myxi_topology_middleware:call(Before),

    %% Assert the exchange.declare wasn't modified
    ?assertEqual(Before#mware.method, After#mware.method),

    %% Assert that only one callback was added
    ?assertEqual(1, length(Post)),

    %% Check the callback is contains the expected arguments
    ?assertEqual({apply, myxi_topology, verify_exchange, [Exchange, Backend]},
                 hd(Post)).

%%
%% Properties
%%

other_amqp_methods_unmodified_test() ->
    ?EQC(?FORALL(M, test_generators:amqp_methods_except(#'exchange.declare'{}),
                 begin
                     Expected = #mware{method = M},
                     Expected =:= myxi_topology_middleware:call(Expected)
                 end)).
