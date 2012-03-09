%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_topology).

-behaviour(gen_server).

-include("include/totochtin.hrl").

%% API
-export([start_link/0,
         add_exchange/2,
         find_exchange/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type state() :: pos_integer().

-define(TABLE, ?MODULE).

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_exchange(Name, Backend) ->
    lager:info("MAP-EXCHANGE ~s -> ~s", [Name, Backend]),
    ets:insert(?TABLE, {Name, Backend, exchange}).

find_exchange(Name) ->
    %% Currently undefined if multiple exchanges declared
    %% on different backends with the same name
    case ets:match(?TABLE, {Name, '$1', exchange}) of
        [[B]|_] -> B;
        []    -> false
    end.

%%
%% Callbacks
%%

-spec init([]) -> {ok, state()}.
%% @hidden
init([]) ->
    lager:info("TOPOLOGY-INIT"),
    process_flag(trap_exit, true),
    {ok, ets:new(?TABLE, [bag, public, named_table])}.

-spec handle_call(_, _, state()) -> {reply, ok, state()}.
%% @hidden
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, state()) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, state(), _) -> {ok, state()}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%


