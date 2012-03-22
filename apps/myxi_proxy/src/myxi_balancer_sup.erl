%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_balancer_sup).

%% RabbitMQ supervisor2 behaviour used to ensure children shutdown
%% per the shudown component of their child_spec
-behaviour(supervisor2).

-include("include/myxi_proxy.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(BALANCER_DELAY, 8000).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} -> {start_balancers(Pid), Pid};
        Error     -> Error
    end.

%%
%% Callbacks
%%

-spec init([]) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init([]) ->
    Spec = {balancer, {myxi_balancer, start_link, []},
            permanent, 2000, worker, [myxi_balancer]},
    {ok, {{simple_one_for_one_terminate, 3, 20}, [Spec]}}.

%%
%% Private
%%

-spec start_balancers(pid()) -> ok.
%% @private
start_balancers(Pid) ->
    %% Used to ensure balancer check starts are delayed
    random:seed(erlang:now()),
    [start(Pid, C) || C <- myxi_config:env(backends, myxi_proxy)],
    ok.

-spec start(pid(), {atom(), options()}) -> {ok, pid} | {error, _}.
%% @private
start(Pid, {Name, Config}) ->
    Args = [Name,
            myxi_config:option(balancer, Config),
            endpoints(Name, Config),
            myxi_config:option(middleware, Config),
            random:uniform(?BALANCER_DELAY)],
    supervisor2:start_child(Pid, Args).

-spec endpoints(atom(), backend()) -> [#endpoint{}].
%% @private
endpoints(Name, Config) ->
    [endpoint(Name, N) || N <- myxi_config:option(nodes, Config)].

-spec endpoint(atom(), options()) -> #endpoint{}.
%% @private
endpoint(Name, Options) ->
    Node = myxi_config:option(node, Options),
    Addr = {myxi_net:hostname(Node), myxi_config:option(port, Options)},
    #endpoint{node    = Node,
              backend = Name,
              address = Addr}.
