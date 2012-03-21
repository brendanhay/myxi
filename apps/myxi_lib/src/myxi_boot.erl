%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_boot).

-include("include/myxi.hrl").

%% API
-export([start/1,
         start/2]).

%%
%% API
%%

-spec start(atom()) -> ok.
%% @doc
start(App) -> start(App, permanent).

-spec start(atom(), application:restart_type()) -> ok.
%% @doc
start(App, Type) -> ensure_started(App, application:start(App, Type)).

%%
%% Private
%%

-spec ensure_started(atom(), ok | {error, {already_started, atom()} | {not_started, atom()}}) -> ok.
%% @private
ensure_started(_App, ok) ->
    ok;
ensure_started(_App, {error, {already_started, _App}}) ->
    ok;
ensure_started(App, {error, {not_started, Dep}}) ->
    start(Dep),
    start(App);
ensure_started(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
