%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%%
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(poxy_sup).

-behaviour(supervisor).

-include("include/poxy.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    {ok, {{one_for_one, 3, 20},
          [balancer_spec(B) || B <- poxy:config(backends)]}}.

%%
%% Private
%%

balancer_spec({Name, Config}) ->
    Mod = poxy:option(balancer, Config),
    Args = [Name,
            Mod,
            node_addresses(Config),
            poxy:option(policies, Config)],
    {Name, {poxy_balancer, start_link, Args},
     permanent, 2000, worker, [poxy_balancer]}.

node_addresses(Config) -> [address(Addr) || Addr <- poxy:option(nodes, Config)].

address(Addr) -> {poxy:option(ip, Addr), poxy:option(port, Addr)}.
