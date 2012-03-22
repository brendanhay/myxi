%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_user_router, [Routes]).

-behaviour(myxi_router).

-include("include/myxi_proxy.hrl").

%% Callbacks
-export([select_balancer/2]).

%%
%% Callbacks
%%

-spec select_balancer(#'connection.start_ok'{}, protocol()) -> atom().
%% @doc
select_balancer(#'connection.start_ok'{response = Response}, Protocol) ->
    User = user(Response, Protocol),
    case [B || [{user, U}, {backend, B}] <- Routes, U =:= User] of
        [Name|_] -> Name;
        []       -> throw({no_backend, User, Routes})
    end.

%%
%% Private
%%

-spec user(binary(), protocol()) -> binary().
%% @private
user(Response, rabbit_framing_amqp_0_9_1) ->
    case extract(Response) of
        {ok, User, _NewResponse} -> User;
        error                    -> throw({failed_to_extract, Response})
    end;
user(Response, rabbit_framing_amqp_0_8) ->
    LoginTable = rabbit_binary_parser:parse_table(Response),
    Key = <<"LOGIN">>,
    case lists:keyfind(Key, 1, LoginTable) of
        {Key, longstr, User} -> User;
        false                -> throw({user_not_found, LoginTable})
    end.

-spec extract(binary() | any()) -> {ok, binary(), binary()} | error.
%% @private
extract(<<0:8, Rest/binary>>) ->
    Count = null_position(Rest, 0),
    <<Elem:Count/binary, Next/binary>> = Rest,
    {ok, Elem, Next};
extract(_) ->
    error.

-spec null_position(binary(), non_neg_integer()) -> non_neg_integer().
%% @private
null_position(<<>>, Count)                  -> Count;
null_position(<<0:8, _Rest/binary>>, Count) -> Count;
null_position(<<_:8, Rest/binary>>,  Count) -> null_position(Rest, Count + 1).
