%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_proxy).

-behaviour(myxi_application).

-include("include/myxi_proxy.hrl").

%% API
-export([start/0,
         stop/0]).

%% Callbacks
-export([start_link/0,
         config/0]).

%%
%% API
%%

-spec start() -> ok.
%% @doc
start() -> myxi_application:start_link(?MODULE).

-spec stop() -> ok.
%% @doc
stop() ->
    myxi_application:terminate(?MODULE),
    init:stop().

add_backend(Config) ->
    ok.

%%
%% Callbacks
%%

-spec start_link() -> {ok, pid()} | {error, _}.
%% @hidden
start_link() ->
    Res = myxi_proxy_sup:start_link(),
    ok = myxi_listener:start_link(),
    Res.

-spec config() -> [#config{}].
%% @hidden
config() -> [].
