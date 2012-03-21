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

-behaviour(application).

-include_lib("myxi_lib/include/myxi.hrl").

%% API
-export([start/0,
         stop/0,
         connect/2,
         establish/2,
         counter/2,
         connections/0]).

%% Callbacks
-export([start/2,
         stop/1]).

%%
%% API
%%

-spec start() -> ok.
%% @doc
start() -> myxi_boot:start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() -> application:stop(?MODULE).

%%
%% Stats
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

-spec start(normal, _) -> {ok, pid()} | {error, _}.
%% @hidden
start(normal, _Args) ->
    case myxi_stats_sup:start_link() of
        ignore -> {error, sup_returned_ignore};
        Ret    -> Ret
    end.

-spec stop(_) -> ok.
%% @hidden
stop(_Args) -> ok.
