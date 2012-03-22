%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_user_router_tests).

-include_lib("myxi/include/myxi_test.hrl").

%%
%% Properties
%%

auth_amqp_plain_test() ->
    ?EQC(?FORALL(
            {User, Backend},
            {myxi_generators:safe_binary(), myxi_generators:safe_atom()},
            ?FORALL(
               Table,
               myxi_generators:amqp_table({<<"LOGIN">>, longstr, User}),
               begin
                   Router = router(User, Backend),
                   Backend =:=
                       Router:select_balancer(
                         start_ok(Table),
                         rabbit_framing_amqp_0_8)
               end))).

auth_plain_test() ->
    ?EQC(?FORALL(
            {User, Pwd, Backend},
            {myxi_generators:safe_binary(), myxi_generators:safe_binary(), myxi_generators:safe_atom()},
            begin
                Router = router(User, Backend),
                Backend =:=
                    Router:select_balancer(
                      start_ok(<<0, User/binary, 0, Pwd/binary>>),
                      rabbit_framing_amqp_0_9_1)
            end)).

%%
%% Helpers
%%

router(User, Backend) ->
    myxi_user_router:new([[{user, User}, {backend, Backend}]]).

start_ok(Bin) when is_binary(Bin) ->
    #'connection.start_ok'{response = Bin};
start_ok(Table) when is_list(Table) ->
    start_ok(rabbit_binary_generator:generate_table(Table)).
