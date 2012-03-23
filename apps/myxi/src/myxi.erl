%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi).

-behaviour(application).

-include("include/myxi.hrl").

%% API
-export([start/0,
         stop/0]).

%% Callbacks
-export([start/2,
         stop/1]).

%%
%% API
%%

-spec start() -> ok.
%% @doc
start() -> start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() ->
    application:stop(?MODULE),
    init:stop().

%%
%% Callbacks
%%

-spec start(normal, _) -> {ok, pid()} | {error, _}.
%% @hidden
start(normal, _Args) ->
    Res = myxi_sup:start_link(),
    ok = myxi_listener:start_link(),
    Res.

-spec stop(_) -> ok.
%% @hidden
stop(_Args) -> ok.

%%
%% Private
%%

-spec start(atom()) -> ok.
%% @doc
start(App) -> ensure_started(App, application:start(App, permanent)).

-spec ensure_started(atom(), ok | {error, {already_started, atom()} |
                                   {not_started, atom()}}) -> ok.
%% @private
ensure_started(_App, ok) ->
    ok;
ensure_started(_App, {error, {already_started, _App}}) ->
    ok;
ensure_started(App, {error, {not_started, Dep}}) ->
    start(Dep),
    start(App);
ensure_started(App, {error, Reason}) ->
    lager:error("Application '~s' start failed with ~p", [App, Reason]),
    error({app_start_failed, App, Reason}).
