%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_roundrobin_balancer).

-behaviour(totochtin_balancer).

-include("include/totochtin.hrl").

%% Callbacks
-export([next/1]).

%%
%% Callbacks
%%

-spec next([#endpoint{}]) -> {#endpoint{} | down, [#endpoint{}]}.
%% @doc
next([])    -> {down, []};
next([H|T]) -> {H, T ++ [H]}.
