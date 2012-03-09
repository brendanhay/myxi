%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_federation_policy).

-behaviour(totochtin_policy).

-include("include/totochtin.hrl").

%% Callbacks
-export([intercept/1]).

%%
%% Callbacks
%%

-spec intercept(#policy{}) -> method().
%% @doc
intercept(Policy = #policy{backend = Backend, method = Method}) ->
    case handle(Backend, Method) of
        false ->
            Policy
    end.

handle(Backend, #'queue.bind'{queue = Queue, exchange = Exchange}) ->
    %% Find which Backend, Exchange lives on
    case totochtin_topology:find_exchange(Exchange) of
        false ->
            lager:error("QUEUE-EX-BIND ~s not found", [Queue, Exchange]),
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

    %% If not the current one, federate
handle(Backend, #'exchange.declare'{exchange = Exchange}) ->
    %% Store the exchange in the topology map
    totochtin_topology:add_exchange(Exchange, Backend),
    false;

handle(_Backend, _Policy) ->
    false.
