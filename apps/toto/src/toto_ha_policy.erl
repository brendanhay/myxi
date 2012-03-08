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

-module(toto_ha_policy).

-behaviour(toto_policy).

-include("include/toto.hrl").

%% Callbacks
-export([modify/1]).

-define(KEY, <<"x-ha-policy">>).

%%
%% Callbacks
%%

-spec modify(method()) -> {modified | unmodified, method()}.
%% @doc
modify(Method = #'queue.declare'{arguments = Args}) ->
    NewArgs = lists:keystore(?KEY, 1, Args, {?KEY, longstr, <<"all">>}),
    {modified, Method#'queue.declare'{arguments = NewArgs}};
modify(Method) ->
    {unmodified, Method}.
