%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_roundrobin_balancer_tests).

-include("include/myxi_test.hrl").

%%
%% Units
%%

next_unavailable_test() ->
    ?assertEqual({down, []}, myxi_roundrobin_balancer:next([])).

%%
%% Properties
%%

next_available_test() ->
    ?EQC(?FORALL(L, non_empty(list()),
                 begin
                     [H|T] = L,
                     {H, T ++ [H]} =:= myxi_roundrobin_balancer:next(L)
                 end)).
