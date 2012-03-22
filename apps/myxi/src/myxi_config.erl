%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_config).

-behaviour(gen_server).

-include("include/myxi.hrl").

%% API
-export([start_link/0,
         load/1,
         env/2,
         option/2]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type message() :: any().

-record(s, {}).

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc Start the config process
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec load([#config{}]) -> ok.
%% @doc
load(Tables) -> ok.

-spec env(atom(), atom()) -> any().
%% @doc
env(Key, App) ->
    case application:load(App) of
        ok              -> ok;
        {error, _Error} -> ok
    end,
    case application:get_env(App, Key) of
        undefined   -> error({config_not_found, Key});
        {ok, Value} -> Value
    end.

-spec option(ip | atom(), options()) ->  inet:ip_address() | any().
%% @doc
option(ip, Opts) ->
    {ok, Ip} = inet:getaddr(lookup_option(ip, Opts), inet),
    Ip;
option(Key, Opts) ->
    lookup_option(Key, Opts).

%%
%% Callbacks
%%

-spec init([]) -> {ok, #s{}}.
%% @hidden
init([]) -> {ok, #s{}}.

-spec handle_call(message(), _, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(message(), #s{}) -> {noreply, #s{}}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_Info, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec call(message()) -> ok.
%% @private
call(Msg) -> gen_server:call(?MODULE, Msg).

-spec cast(message()) -> ok.
%% @private
cast(Msg) -> gen_server:cast(?MODULE, Msg).

-spec lookup_option(atom(), options()) -> any().
%% @private
lookup_option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.
