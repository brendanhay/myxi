%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_ha_policy).

-behaviour(myxi_policy).

-include("include/myxi.hrl").

%% Callbacks
-export([intercept/1]).

-define(KEY, <<"x-ha-policy">>).

%%
%% Callbacks
%%

-spec intercept(#policy{}) -> method().
%% @doc
intercept(Policy = #policy{method = Method}) ->
    case Method of
         #'queue.declare'{queue = Q, arguments = Args} ->
            lager:info("HA-POLICY ~s", [Q]),
            NewArgs = lists:keystore(?KEY, 1, Args, {?KEY, longstr, <<"all">>}),
            Policy#policy{method = Method#'queue.declare'{arguments = NewArgs}};
        _Other ->
            Policy
    end.
