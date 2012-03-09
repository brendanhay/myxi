%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_policy).

-include("include/totochtin.hrl").

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([handler/4]).

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{intercept, 1}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{intercept, 1}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

-spec handler(atom(), ets:tid(), protocol(), [policy()])
             -> fun((method()) -> method() | false).
%% @doc
handler(Current, Topology, Protocol, Policies) ->
    Fn = compose([fun(A) -> P:intercept(A) end || P <- Policies]),
    Args = #policy{backend = Current, topology = Topology, protocol = Protocol},
    fun(M) -> compare(M, Fn(Args#policy{method = M})) end.
%%
%% Private
%%

-spec compose([fun()]) -> fun().
%% @private
compose(Fns) -> lists:foldl(fun compose/2, fun(X) -> X end, Fns).

-spec compose(fun(), fun()) -> fun().
%% @private
compose(F, G) -> fun(X) -> F(G(X)) end.

-spec compare(method(), #policy{}) -> method() | false.
%% @private
compare(Original, #policy{method = New}) ->
    case New =/= Original of
        true  -> New;
        false -> false
    end.

