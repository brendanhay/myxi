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

init_per_suite(Config) ->
    meck:new(Mocks = [gen_tcp, myxi_connection], [unstick, passthrough, no_link]),
    [{mocks, Mocks}|Config].

end_per_suite(Config) ->
    meck:unload(?config(mocks, Config)),
    suite_helper:clear([mocks], Config).

all() -> [replay_handshake].

%%
%% Tests
%%

replay_handshake(_Config) ->
    %% Setup myxi_connection:replay/4 to send a message to the caller
    Replay = fun(Pid, _, _, _) -> Pid ! replayed, ok end,
    meck:expect(myxi_connection, replay, Replay),

    %% Mock other connection calls since they are used by the frontend
    meck:expect(myxi_connection, reply, 4, ok),
    meck:expect(myxi_connection, forward, 5, ok),

    %% Mock the client socket to send the binaries composing an
    %% AMQP handshake
    recv_sequence(amqp_handshake()),

    %% Fire up the frontend and wait for the mocked replay message
    {ok, Pid} = myxi_frontend:start_link(self(), sock),
    receive replayed -> ok end,

    %% Ensure the replay to the server is identical to the received handshake
    Expected = [self(), start_ok(), '_', rabbit_framing_amqp_0_9_1],
    ?assert(meck:called(myxi_connection, replay, Expected)).

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
