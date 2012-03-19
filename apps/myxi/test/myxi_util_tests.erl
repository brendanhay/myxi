%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_util_tests).

-include("include/myxi_test.hrl").

%%
%% Fixtures
%%

%%
%% Properties
%%

bin_test() ->
    ?EQC(?FORALL(T, union([binary(), atom(), myxi_generators:word()]),
                 is_binary(myxi_util:bin(T)))).

split_host_defaults_test() ->
    ?EQC(?FORALL({H, D}, {myxi_generators:word(), atom()},
                 begin
                     {H, D} =:= myxi_util:split_host(H, D)
                 end)).

split_host_with_port_test() ->
    ?EQC(?FORALL({H, P}, {myxi_generators:word(), pos_integer()},
                 begin
                     Host = H ++ ":" ++ integer_to_list(P),
                     {H, P} =:= myxi_util:split_host(Host)
                 end)).
