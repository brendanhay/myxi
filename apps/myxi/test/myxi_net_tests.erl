%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_net_tests).

-include("include/myxi_test.hrl").

%%
%% Properties
%%

split_host_test_() ->
    [{"Host with no port number returns default",
      ?_EQC(?FORALL({H, D}, {test_generators:word(), atom()},
                    {H, D} =:= myxi_net:parse(H, D)))},

     {"Host with port returns port number",
      ?_EQC(?FORALL({H, P}, {test_generators:word(), pos_integer()},
                    begin
                        Host = H ++ ":" ++ integer_to_list(P),
                        {H, P} =:= myxi_net:parse(Host)
                    end))}].
