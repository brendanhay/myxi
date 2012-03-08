%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%%
%% A copy of the MPL can be found in LICENSE in the top level or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(poxy_balancer).

-include("include/poxy.hrl").

-behaviour(gen_server).

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([start_link/4,
         next/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {mod          :: module(),
            addrs        :: [addr()],
            policies :: [policy()]}).
%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{next, 1}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{next, 1}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

start_link(Name, Mod, Addrs, Policies) ->
    lager:info("BALANCE-START ~s ~s ~p", [Name, Mod, Addrs]),
    State = #s{mod = Mod, addrs = Addrs, policies = Policies},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

-spec next(pid()) -> {addr(), [policy()]}.
%% @doc
next(Pid) -> gen_server:call(Pid, next).

%%
%% Callbacks
%%

init(State) ->
    process_flag(trap_exit, true),
    {ok, State}.

-spec handle_call(next, reference(), #s{}) -> {reply, {addr(), [policy()]}, #s{}}.
%% @hidden
handle_call(next, _From, State = #s{mod          = Mod,
                                    addrs        = Addrs,
                                    policies = Policies}) ->
    {Addr, NewAddrs} = Mod:next(Addrs),
    lager:info("BALANCE-NEXT ~s ~s", [Mod, poxy:format_ip(Addr)]),
    {reply, {Addr, Policies}, State#s{addrs = NewAddrs}}.

-spec handle_cast(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, #s{}) -> {noreply, #s{}} | {stop, normal, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.
