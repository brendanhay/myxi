%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(federation_policy).

%% -behaviour(myxi_policy).

%% -include("include/myxi.hrl").

%% %% Callbacks
%% -export([intercept/1]).

%% %%
%% %% Callbacks
%% %%

%% -spec intercept(#policy{}) -> method().
%% %% @doc
%% intercept(Policy = #policy{backend = Backend, addr = Addr, method = Method}) ->
%%     case handle({Backend, Addr}, Method) of
%%         false ->
%%             Policy
%%     end.

%% handle({Backend, Addr}, #'queue.bind'{queue = Queue, exchange = Exchange}) ->
%%     %% Find which Backend, Exchange lives on
%%     case myxi_topology:find_exchange(Exchange) of
%%         false ->
%%             lager:error("FED-EX-BIND ~s not found", [Queue, Exchange]);
%%         Backend ->
%%             %% Same, alles ok
%%             lager:notice("FED-EXIST ~s found on ~s", [Exchange, Backend]);
%%         Other ->
%%             %% Connect to addr via direct erlang client + setup federation
%%             lager:notice("FED-FEDERATE ~s found on ~s but current is ~s",
%%                          [Exchange, Other, Backend]),

%%             %% Connect via distributed erlang to Addr

%%             %% Get exact copy of exchange

%%             %% Declare exchange locally, as x-federation with
%%             %% upstream_set as Other

%%             %% Let the queue.bind go through
%%     end,
%%     false;

%% handle({Backend, _Addr}, #'exchange.declare'{exchange = Exchange}) ->
%%     %% Store the exchange declaration in the topology map
%%     myxi_topology:add_exchange(Exchange, Backend),
%%     false;

%% handle(_Backend, _Policy) ->
%%     false.

%% Topology map:

%% Can't be done through state that is pre-empted by calls as
%% I have no idea if the call succeeds.
%% Need to actually call to rabbit ..


%% Need boot steps before accepting connections





%% During boot do a multicall to populate exchange map

%% On new exchange.declares verify with the backend
%% And create a lock
%% and then release the lock when the map is updated

%% Try to obtain a lock when calling queue.bind





%% A:
%% Store an mnesia bag of {exchange, backend} tuples
%% Lookup the exchange name in backend using distributed erlang
%% + rabbit_exchange:lookup/1
%% if it does't exist, remove it from the topology and let the call through
%% otherwise declare the exchange in the current backend, but as x-federation
%% + transformed type/args

%% Check the mnesia bag,
%%  if it doesn't exist check all backends except the current one

%% Multiple vhosts? - Will need to store the vhost from start_ok

%% %% Excerpt from rabbit_exchange.erl
%% lookup(Name) ->
%%     rabbit_misc:dirty_read({rabbit_exchange, Name}).

%% lookup_or_die(Name) ->
%%     case lookup(Name) of
%%         {ok, X}            -> X;
%%         {error, not_found} -> rabbit_misc:not_found(Name)
%%     end.

%% B:
%% Rabbit Plugin which pushes exchange declarations to a store
%% Custom exchange type which is added by policy from the proxy
%% which fires back create/delete events

%% C:
%% Event sink which duplicates and parses the stream returned from
%% the backend, looking for exchange.declare_ok _matches_ to previously
%% forwarded exchange.declares/delete

%% D:
%% Use the management webmachine resource to poll the exchanges

%% E:
%% rabbit_exchange:info_all(VHostPath)
%% name, type, durable, auto_delete, internal, arguments
%% Can be used to initially populate for A:
%% Could also be used to throw a timer which verifies the exchange.declare
%% Succeeded in A:



