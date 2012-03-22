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

-include_lib("myxi/include/myxi.hrl").

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

%%
%% Callbacks
%%

-spec start_link() -> {ok, pid()} | {error, _}.
%% @hidden
start_link() -> myxi_proxy_sup:start_link().

-spec config() -> [#config{}].
%% @hidden
config() -> [].

%%
%% Setup
%%

-spec start_listeners() -> ok.
%% @private
start_listeners() -> lists:foreach(fun listener/1, myxi_util:config(frontends)).

-spec listener(frontend()) -> {ok, pid()}.
%% @private
listener(Config) ->
    Tcp = tcp_options(Config),
    lager:info("LISTEN ~s", [myxi_util:format_ip(Tcp)]),
    cowboy:start_listener(amqp_listener, myxi_util:option(max, Config),
                          cowboy_tcp_transport, Tcp,
                          myxi_connection, Config).

-spec tcp_options(frontend()) -> [proplists:property()].
%% @private
tcp_options(Config) ->
    [{ip, myxi_util:option(ip, Config)},
     {port, myxi_util:option(port, Config)}|myxi_util:config(tcp)].
