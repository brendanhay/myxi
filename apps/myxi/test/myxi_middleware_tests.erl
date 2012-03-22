%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_middleware_tests).

-include("/include/myxi_test.hrl").

-define(MOCK, myxi_mw_test).

%%
%% Fixtures
%%

setup() ->
    meck:new(?MOCK),
    [?MOCK].

teardown(Mocks) ->
    meck:unload(Mocks).

compose_middleware_test_() ->
    Fn = myxi_middleware:wrap(#endpoint{}, amqp_protocol, [?MOCK]),

    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Returns unmodified if method unchanged",
       ?_test(
          begin
              meck:expect(?MOCK, call, fun(MW) -> MW end),
              ?assertMatch({unmodified, [], []}, Fn(#'queue.declare'{}))
          end)},

      {"Returns new method if method changed",
       ?_test(
          begin
              meck:expect(?MOCK, call, fun(MW) -> MW#mware{method = #'basic.ack'{}} end),
              ?assertMatch({#'basic.ack'{}, [], []}, Fn(#'queue.declare'{}))
          end)},

      {"Returns pre callbacks",
       ?_test(
          begin
              Pre = [3, 1, 2],
              meck:expect(?MOCK, call, fun(MW) -> MW#mware{pre = Pre} end),
              ?assertMatch({_, Pre, []}, Fn(#'basic.get'{}))
          end)},

      {"Returns post callbacks",
       ?_test(
          begin
              Post = [d, g, z, q],
              meck:expect(?MOCK, call, fun(MW) -> MW#mware{post = Post} end),
              ?assertMatch({_, [], Post}, Fn(#'tx.select'{}))
          end)}
     ]}.
