%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_frontend_SUITE).

-include("myxi_common_test.hrl").

-define(BALANCER, myxi_dummy_balancer).
-define(UP,       check_up).
-define(DOWN,     check_down).

%%
%% Callbacks
%%

init_per_testcase(_Case, Config) ->
    meck:new(gen_tcp, [unstick, passthrough]),
    [{mocks, [gen_tcp]}, {frontend, none}|Config].

end_per_testcase(_Case, Config) ->
    %% meck:unload(?config(mocks, Config)),
    suite_helper:clear([mocks], Config).

all() -> [replay_handshake].

%%
%% Tests
%%

replay_handshake(_Config) ->
    lager:start(),
    meck:new(myxi_connection),
    meck:expect(myxi_connection, reply, 4, ok),
    Self = self(),
    meck:expect(myxi_connection, replay, fun(_, _, _, _) -> Self ! replayed, ok end),
    meck:expect(myxi_connection, forward, 5, ok),
    recv_sequence(amqp_handshake()),
    {ok, Pid} = myxi_frontend:start_link(self(), sock),
    receive replayed -> ok end,
    ?assert(meck:called(myxi_connection, replay, [Self, start_ok(), '_', rabbit_framing_amqp_0_9_1])).

%%
%% Helpers
%%

amqp_handshake() ->
    Frame =
        rabbit_binary_generator:build_simple_method_frame(
          0, start_ok(), rabbit_framing_amqp_0_9_1),
    [{ok, <<"AMQP", 0, 0, 9, 1>>},
     {ok, iolist_to_binary(Frame)}].

start_ok() ->
    #'connection.start_ok'{
          client_properties = [],
          response  = <<0, "guest", 0, "guest">>
         }.

recv_sequence(Sequence) -> meck:sequence(gen_tcp, recv, 2, Sequence).
