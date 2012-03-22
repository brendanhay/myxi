%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_stats).

-behaviour(myxi_application).

-include("include/myxi_stats.hrl").

%% API
-export([counter/2,
         timer/2]).

%% Callbacks
-export([start_link/0,
         config/0]).

%%
%% API
%%

-spec counter(atom() | string(), pos_integer()) -> ok.
%% @doc
counter(Bucket, Step) -> myxi_stats_server:cast({counter, Bucket, Step}).

-spec timer(atom() | string(), pos_integer()) -> ok.
%% @doc
timer(Bucket, Ms) -> myxi_stats_server:cast({timer, Bucket, Ms}).

%%
%% Callbacks
%%

-spec start_link() -> {ok, pid()} | {error, _}.
%% @hidden
start_link() -> myxi_stats_sup:start_link().

-spec config() -> [#config{}].
%% @hidden
config() -> [].
