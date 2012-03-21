%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_federation_middleware).

-behaviour(myxi_middleware).

-include_lib("myxi_lib/include/myxi.hrl").

%% Callbacks
-export([call/1]).

%%
%% Callbacks
%%

-spec call(#mware{}) -> #mware{}.
%% @doc
call(MW = #mware{endpoint = Endpoint, method = Method, pre = Pre}) ->
    case Method of
        #'queue.bind'{exchange = Exchange} ->
            case pre(Endpoint, Exchange) of
                false -> MW;
                Add   -> MW#mware{pre = Pre ++ Add}
            end;
        _Other ->
            MW
    end.

%%
%% Private
%%

-spec pre(#endpoint{}, binary()) -> [action()] | false.
%% @private
pre(#endpoint{node = Node, backend = Backend}, Exchange) ->
    do([truth_m ||
           Matches <- myxi_topology:find_exchange(Exchange),
           case lists:keyfind(Backend, 1, Matches) of
               false   -> return(continue);
               _Exists -> false
           end,
           [{Other, Declare}] <- [{O, D} || {O, D} <- Matches, not federated(D)],
           upstream_exists(Exchange, Other, Node),
           create_link(Declare, Other, Backend, Node)]).

-spec federated(#'exchange.declare'{}) -> true | false.
%% @private
federated(#'exchange.declare'{type = <<"x-federation">>}) -> true;
federated(_Other)                                         -> false.

-spec upstream_exists(binary(), atom(), node()) -> true | false.
%% @private
upstream_exists(Exchange, Upstream, Node) ->
    lager:error("FED-TRY upstream_set: ~p", [Upstream]),
    %% from_set only looks at #resrouce.name/.vhost
    Name = #resource{name = Exchange, virtual_host = <<"/">>},
    Set = atom_to_binary(Upstream, latin1),
    case rpc:call(Node, rabbit_federation_upstream, from_set, [Set, Name]) of
        {ok, _Any} ->
            true;
        Error ->
            lager:error("FED-FAILURE upstream_set: ~p", [Error]),
            false
    end.

-spec create_link(#'exchange.declare'{}, atom(), atom(), node()) -> [action()].
%% @private
create_link(Declare, Upstream, Downstream, Node) ->
    Send = declaration(Declare, Upstream),
    Params = #amqp_params_direct{username = myxi_util:bin(Downstream), node = Node},
    [{fn, fun() -> send(Send, Params) end}].

-spec declaration(#'exchange.declare'{}, atom()) -> #'exchange.declare'{}.
%% @private
declaration(Declare = #'exchange.declare'{type = Type, arguments = Args}, Upstream) ->
    Declare#'exchange.declare'{
      type = <<"x-federation">>,
      arguments = args(Args, Type, Upstream)
     }.

-spec args([tuple()], list() | binary(), atom()) -> [tuple()].
%% @private
args(Args, Type, Upstream) ->
    Merge = [{<<"upstream-set">>, longstr, myxi_util:bin(Upstream)},
             {<<"type">>, longstr, myxi_util:bin(Type)}],
    lists:keymerge(1, Args, Merge).

-spec send(method(), #amqp_params_direct{}) -> error_m(ok, term()).
%% @private
send(Method, Params) ->
    do([error_m ||
           Conn <- amqp_connection:start(Params),
           Chan <- amqp_connection:open_channel(Conn),
           return(amqp_channel:call(Chan, Method)),
           return(amqp_channel:close(Chan)),
           amqp_connection:close(Conn)]).
