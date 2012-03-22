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

-include_lib("myxi/include/myxi.hrl").

%% API
-export([connect/2,
         establish/2,
         counter/2,
         connections/0]).

%% Callbacks
-export([start_link/0,
         config/0]).

%%
%% API
%%

-spec connect(pid(), inet:socket()) -> ok.
%% @doc
connect(Conn, Client) ->
    myxi_stats_server:cast({connect, Conn, Client, now()}).

-spec establish(pid(), #endpoint{}) -> ok.
%% @doc
establish(Conn, #endpoint{node = Node}) ->
    myxi_stats_server:cast({establish, Conn, Node}).

-spec counter(atom(), pos_integer()) -> ok.
%% @doc
counter(Stat, Step) ->
    myxi_stats_server:cast({counter, Stat, Step}).

-spec connections() -> [{pid(), non_neg_integer()}].
%% @doc
connections() ->
    myxi_stats_server:call(connections).

%%
%% Callbacks
%%

-spec start_link() -> {ok, pid()} | {error, _}.
%% @hidden
start_link() -> myxi_stats_sup:start_link().

-spec config() -> [#config{}].
%% @hidden
config() -> [].
