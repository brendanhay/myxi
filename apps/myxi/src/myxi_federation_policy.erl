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
-export([inject/1]).

%%
%% Callbacks
%%

-spec inject(#policy{}) -> #policy{}.
%% @doc
inject(Policy = #policy{endpoint = Endpoint,
                        method   = #'queue.bind'{exchange = Exchange},
                        pre      =  Pre}) ->
    Policy#policy{pre = pre_commands(Endpoint, Exchange) ++ Pre};
inject(Policy) ->
    Policy.

%%
%% Private
%%

-spec pre_commands(#endpoint{}, binary()) -> [action()].
%% @private
pre_commands(#endpoint{node = Node, backend = Backend}, Exchange) ->
    case myxi_topology:find_exchange(Exchange) of
        false ->
            [];
        {Backend, _Declare} ->
            [];
        {Other, Declare} ->
            case upstream_exists(Node, Exchange, Other) of
                true  -> federate(Declare, Other);
                false -> []
            end
    end.

-spec federate(#'exchange.declare'{}, atom()) -> [action()].
%% @private
federate(Declare = #'exchange.declare'{type = Type, arguments = Args}, Upstream) ->
    Send = Declare#'exchange.declare'{
             type      = <<"x-federation">>,
             arguments = args(Args, Type, Upstream)
            },
    [{send, Send}, {recv, #'exchange.declare_ok'{}}].

-spec args([tuple()], list() | binary(), atom()) -> [tuple()].
%% @private
args(Args, Type, Upstream) ->
    Merge = [{<<"upstream_set">>, longstr, myxi:bin(Upstream)},
             {<<"type">>, longstr, myxi:bin(Type)}],
    myxi:merge_keylist(Args, Merge).

-spec upstream_exists(node(), binary(), atom()) -> true | false.
%% @private
upstream_exists(Node, Exchange, Upstream) ->
    %% from_set only looks at #resrouce.name/.vhost
    Name = #resource{name = Exchange, virtual_host = <<"/">>},
    Set = list_to_binary(atom_to_list(Upstream)),
    case rpc:call(Node, rabbit_federation_upstream, from_set, [Set, Name]) of
        {error, Error} ->
            lager:error("FED-FAILURE upstream_set: ~p", [Error]),
            false;
        {ok, _Any} ->
            true
    end.
