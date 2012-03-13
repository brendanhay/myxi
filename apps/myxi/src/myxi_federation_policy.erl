%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_federation_policy).

-behaviour(myxi_policy).

-include("include/myxi.hrl").

%% Callbacks
-export([intercept/1]).

-define(VERIFY_DELAY, 500).

%%
%% Callbacks
%%

-spec intercept(#policy{}) -> method().
%% @doc
intercept(Policy = #policy{backend = Backend, method = Method, callbacks = Callbacks}) ->
    case handle(Method, Backend) of
        {false, AddCallbacks} ->
            Policy#policy{callbacks = Callbacks ++ AddCallbacks};
        false ->
            Policy
    end.

%%
%% Private
%%

handle(#'queue.bind'{queue = Queue, exchange = Exchange}, Backend) ->
    %% Find which Backend, Exchange lives on
    case myxi_topology:find_exchange(Exchange) of
        false ->
            lager:error("QUEUE-EX-BIND ~s not found", [Exchange]),
            false;
        Backend ->
            %% Same, alles ok
            lager:notice("FED-EXIST ~s found on ~s", [Exchange, Backend]),
            false;
        Other ->
            %% Federate!
            lager:notice("FED-FEDERATE ~s found on ~s but current is ~s",
                         [Exchange, Other, Backend]),
            false
    end;

handle(#'exchange.declare'{exchange = Name}, Backend) ->
    {false, [fun() -> myxi_topology:verify_exchange(Name, Backend) end]};

handle(_NoMatch, _Backend) ->
    false.
