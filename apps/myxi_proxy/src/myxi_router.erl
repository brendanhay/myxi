%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_router).

-include_lib("myxi/include/myxi.hrl").

%% API
-export([new/1,
         route/3]).

%%
%% Behaviour
%%

-callback select_balancer(#'connection.start_ok'{}, protocol()) -> atom().

%%
%% API
%%

-spec new(frontend()) -> module().
%% @doc
new(Frontend) ->
    {router, Mod, Routes} = lists:keyfind(router, 1, Frontend),
    Mod:new(Routes).

-spec route(router(), #'connection.start_ok'{}, protocol()) -> myxi_balancer:next().
%% @doc
route(Router, StartOk, Protocol) ->
    Balancer = Router:select_balancer(StartOk, Protocol),
    myxi_balancer:next(Balancer).
