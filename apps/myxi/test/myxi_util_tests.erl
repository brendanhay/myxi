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
