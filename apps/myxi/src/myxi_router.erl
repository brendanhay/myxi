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

-include("include/myxi.hrl").

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([new/1,
         route/3]).

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{select_balancer, 2}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{select_balancer, 2}];
behaviour_info(_Other)    -> undefined.

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
