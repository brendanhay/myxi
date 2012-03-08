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

-module(toto_roundrobin_balancer).

-behaviour(toto_balancer).

-include("include/toto.hrl").

%% Callbacks
-export([next/1]).

%%
%% Callbacks
%%

-spec next([addr()]) -> {addr(), [addr()]}.
%% @doc
next([H|T]) -> {H, T ++ [H]}.
