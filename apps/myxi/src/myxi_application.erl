%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_application).

-behaviour(application).

-include("include/myxi.hrl").

%% API
-export([start_link/1,
         start_link/2,
         terminate/1]).

%% Callbacks
-export([start/2,
         stop/1]).

%%
%% Behaviour
%%

-callback start_link() -> {ok, pid()} | {error, _}.
-callback config()     -> [#config{}].

%%
%% API
%%

-spec start_link(atom()) -> ok.
%% @doc
start_link(App) -> start_link(App, permanent).

-spec start_link(atom(), application:restart_link_type()) -> ok.
%% @doc
start_link(App, Restart) ->
    ok = myxi_config:load(App),
    ensure_started(App, application:start(App, Restart)).

-spec terminate(atom()) -> ok.
%% @doc
terminate(App) -> application:stop(App).

%%
%% Callbacks
%%

-spec start(normal, [atom()]) -> {ok, pid()} | {error, _}.
%% @hidden
start(normal, [App]) ->
    case App:start_link() of
        ignore -> {error, sup_returned_ignore};
        Ret    -> Ret
    end.

-spec stop(_) -> ok.
%% @hidden
stop(_Args) -> ok.

%%
%% Private
%%

-spec ensure_started(atom(), ok | {error, {already_started, atom()} |
                                   {not_started, atom()}}) -> ok.
%% @private
ensure_started(_App, ok) ->
    ok;
ensure_started(_App, {error, {already_started, _App}}) ->
    ok;
ensure_started(App, {error, {not_started, Dep}}) ->
    start_link(Dep),
    start_link(App);
ensure_started(App, {error, Reason}) ->
    lager:error("Application '~s' start failed with ~p", [App, Reason]),
    erlang:error({app_start_failed, App, Reason}).
