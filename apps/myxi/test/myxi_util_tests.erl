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

split_host_test_() ->
    [{"Host with no port number returns default",
      ?_EQC(?FORALL({H, D},
                    {myxi_generators:word(), atom()},
                    {H, D} =:= myxi_util:split_host(H, D)))},

     {"Host with port returns port number",
      ?_EQC(?FORALL({H, P},
                    {myxi_generators:word(), pos_integer()},
                    begin
                        Host = H ++ ":" ++ integer_to_list(P),
                        {H, P} =:= myxi_util:split_host(Host)
                    end))}].

os_env_test_() ->
    {foreach,
     fun() ->
             meck:new(myxi_util, [passthrough]),
             meck:expect(myxi_util, os_env, 1, false),
             [myxi_util]
     end,
     fun meck:unload/1,
     [{"String returns same string",
       ?_EQC(?FORALL({K, D},
                     {myxi_generators:word(), any()},
                     K =:= myxi_util:os_env(K, D)))},

      {"Non-existing variable returns default",
       ?_EQC(?FORALL({K, D},
                     {myxi_generators:safe_atom(), any()},
                     D =:= myxi_util:os_env(K, D)))}

      %% {"Existing variable returns value",
      %%  ?_EQC(?FORALL({K, D, V},
      %%                {myxi_generators:safe_atom(), any(), myxi_generators:word()},
      %%                begin
      %%                    meck:expect(myxi_util, os_env, fun(A) -> throw({err, A}) end),
      %%                    V =:= myxi_util:os_env(K, D)
      %%                end))}
     ]}.
