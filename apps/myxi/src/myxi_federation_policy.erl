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

-define(CONN_PARAMS(Node), #amqp_params_direct{username = <<"guest">>,
                                               node     = Node}).

%%
%% Callbacks
%%

-spec inject(#policy{}) -> method().
%% @doc
inject(Policy = #policy{endpoint  = Endpoint,
                        method    = Method,
                        callbacks = Callbacks}) ->
    case handle(Method, Endpoint) of
        {false, AddCallbacks} ->
            Policy#policy{callbacks = Callbacks ++ AddCallbacks};
        false ->
            Policy
    end.

%%
%% Private
%%

handle(#'queue.bind'{exchange = Name},
       _Endpoint = #endpoint{node = Node, backend = Backend}) ->
    %% Find which Backend, Name lives on
    case myxi_topology:find_exchange(Name) of
        false ->
            %% Temporarily asserting that the exchange must exist
            error({federation_policy, bind_failed});
        {Backend, _Declare} ->
            false;
        {Other, Declare} ->
            declare(federated(Declare, Other), Node),
            %% return false so the bind goes through
            false
    end;

handle(#'exchange.declare'{exchange = Name}, #endpoint{backend = Backend}) ->
    {false, [fun() -> myxi_topology:verify_exchange(Name, Backend) end]};

handle(_NoMatch, _Backend) ->
    false.

federated(Declare = #'exchange.declare'{type = Type, arguments = Args}, Set) ->
    NewArgs = merge_keylist(Args, [{type, Type}, {upstream_set, Set}]),
    Declare#'exchange.declare'{type = <<"x-federation">>, arguments = NewArgs}.

merge_keylist(L1, L2) ->
    Fold = fun({K, V}, A) -> lists:keystore(K, 1, A, {K, V}) end,
    lists:foldl(Fold, L1, L2).

declare(Declare, Node) ->
    lager:notice("FED-START ~p on ~p", [Declare, Node]),
    {ok, Conn} = amqp_connection:start(?CONN_PARAMS(Node)),
    {ok, Chan} = amqp_connection:open_channel(Conn),
    try
        #'exchange.declare_ok'{} = amqp_channel:call(Chan, Declare),
        lager:notice("FED-SUCCESS ~s declared on ~p",
                     [Declare#'exchange.declare'.exchange, Node]),
        amqp_channel:close(Chan),
        amqp_connection:close(Conn)
    catch
        exit:Reason ->
            lager:error("FED-FAILURE ~p", [Reason])
    end.
