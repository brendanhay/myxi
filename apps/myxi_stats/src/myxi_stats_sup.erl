%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_stats_sup).

-behaviour(supervisor).

-include("include/myxi_stats.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    Spec = {stats, {myxi_stats_server, start_link, server_args()},
            permanent, 2000, worker, [myxi_stats_server]},
    {ok, {{one_for_all, 3, 20}, [Spec]}}.

%%
%% Private
%%

-spec server_args() -> [string()].
%% @private
server_args() ->
    Config = myxi_config:env(statsd, myxi_stats),
    [myxi_config:option(namespace, Config),
     myxi_os:env(myxi_config:option(url, Config), "localhost:8126")].
