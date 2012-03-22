%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_os_tests).

-include("include/myxi_test.hrl").

%%
%% Properties
%%

os_env_test_() ->
    {foreach,
     fun() ->
             meck:new(myxi_util, [passthrough]),
             meck:expect(myxi_util, os_env, 1, false),
             [myxi_util]
     end,
     fun meck:unload/1,
     [{"String returns same string",
       ?_EQC(?FORALL({K, D}, {test_generators:word(), any()},
                     K =:= myxi_os:env(K, D)))},

      {"Non-existing variable returns default",
       ?_EQC(?FORALL({K, D}, {test_generators:safe_atom(), any()},
                     D =:= myxi_os:env(K, D)))}]}.
