%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%%
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_policy).

-include("include/totochtin.hrl").

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([thrush/2]).

-type modified() :: modified | unmodified.

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{modify, 1}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{modify, 1}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

-spec thrush(method(), [policy()]) -> {modified(), method()}.
%% @doc
thrush(Method, Policies) ->
    lists:foldl(fun modify/2, {unmodified, Method}, Policies).

%%
%% Private
%%

-spec modify(policy(), {modified(), method()}) -> {modified(), method()}.
%% @private
modify(I, {modified, M}) ->
    {_, N} = I:modify(M),
    {modified, N};
modify(I, {unmodified, M}) ->
    I:modify(M).
