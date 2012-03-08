%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
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
-type return()   :: {modified(), method()}.

-export_types([modified/0,
               return/0]).

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{modify, 3}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{modify, 3}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

-spec thrush(atom(), method(), [{atom(), pid(), policy()}]) -> return().
%% @doc
thrush(Method, Policies) ->
    lists:foldl(fun modify/2, {unmodified, Method}, Policies).

%%
%% Private
%%

-spec modify({atom(), pid(), policy()}, {modified(), method()}) -> return().
%% @private
modify({Current, Topology, Policy}, {unmodified, Method}) ->
    Policy:modify(Method, Current, Topology);
modify({Current, Topology, Policy}, {modified, Method}) ->
    {_, NewMethod} = Policy:modify(Method, Current, Topology),
    {modified, NewMethod}.
