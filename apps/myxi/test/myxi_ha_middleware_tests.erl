%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_ha_middleware_tests).

-include("include/myxi_test.hrl").

%%
%% Units
%%

add_ha_queue_arguments_test() ->
    Before = #mware{method = #'queue.declare'{}},
    After = #mware{method = Method = #'queue.declare'{arguments = Args}} =
        myxi_ha_middleware:call(Before),

    %% Assert that the arguments have been added
    ?assertEqual([{<<"x-ha-policy">>, longstr, <<"all">>}], Args),

    %% Reset the arguments to 'Before', and assert equality
    Reset = After#mware{method = Method#'queue.declare'{arguments = []}},
    ?assertEqual(Before, Reset).

%%
%% Properties
%%

other_amqp_methods_unmodified_test() ->
    ?EQC(?FORALL(M, myxi_generators:amqp_methods_except(#'queue.declare'{}),
                 begin
                     Expected = #mware{method = M},
                     Expected =:= myxi_ha_middleware:call(Expected)
                 end)).
