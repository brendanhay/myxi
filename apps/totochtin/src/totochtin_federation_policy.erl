%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_federation_policy).

-behaviour(totochtin_policy).

-include("include/totochtin.hrl").

%% Callbacks
-export([modify/1]).

%%
%% Callbacks
%%

-spec modify(atom(), pid(), method()) -> totochtin_policy:return().
%% @doc
modify(Method = #'queue.declare'{queue = Queue}) ->
    {unmodified, Method};

modify(Method = #'queue.bind'{queue = Queue, exchange = Exchange}) ->
    {unmodified, Method};

modify(Method) ->
    {unmodified, Method}.
