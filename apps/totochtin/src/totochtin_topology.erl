%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_topology).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/totochtin.hrl").

%% API
-export([start_link/0,
         %% add_exchange/2,
         find_exchange/1,
         verify_exchange/2,
         add_endpoints/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type state() :: pos_integer().

-record(e, {name    :: binary(),
            backend :: atom(),
            declare :: #'exchange.declare'{}}).

-define(TABLE, ?MODULE).

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% add_exchange(Declare = #'exchange.declare'{exchange = Name}, Backend) ->
%%     lager:info("MAP-EXCHANGE ~s -> ~s", [Name, Backend]),
%%     ets:insert(?TABLE, #e{name = Name, backend = Backend, declare = Declare}).

find_exchange(Name) ->
    %% Currently undefined if multiple exchanges declared
    %% on different backends with the same name
    case ets:match(?TABLE, #e{name = Name, backend = '$1',  _ = '_'}) of
        [[B]|_] -> B;
        []    -> false
    end.

%% Get a node from the balencer/backend and then find the exchange,
%% loading it into ets if succcessful
verify_exchange(Name, Backend) ->
    lager:info("VERIFY-EXCHANGE ~s -> ~s", [Name, Backend]),
    {#endpoint{node = Node}, _Policies} = totochtin_balancer:next(Backend),
    Resource = rabbit_misc:r(<<"/">>, exchange, Name),
    case rpc:call(Node, rabbit_misc, dirty_read, [{rabbit_exchange, Resource}]) of
        {ok, Exchange} ->
            case default_exchange(Exchange) of
                true ->
                    true;
                false ->
                    ets:insert(?TABLE, #e{name    = Name,
                                          backend = Backend,
                                          declare = declare(Exchange)})
            end;
        {error, not_found} ->
            false
    end.

-spec add_endpoints([#endpoint{}]) -> ok.
%% @private
add_endpoints(Endpoints) ->
    Exchanges = lists:flatten([list_exchanges(E) || E <- Endpoints]),
    lager:info("TOPOLOGY-INS ~p", [Exchanges]),
    ets:insert(?TABLE, Exchanges).

%%
%% Callbacks
%%

-spec init([]) -> {ok, state()}.
%% @hidden
init([]) ->
    lager:info("TOPOLOGY-INIT"),
    process_flag(trap_exit, true),
    {ok, ets:new(?TABLE, [bag, public, named_table, {keypos, #e.name}])}.

-spec handle_call(info, _, state()) -> {reply, ok, state()}.
%% @hidden
handle_call(info, _From, State) ->
    {reply, ets:match(?TABLE, '$1'), State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, state()) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, state(), _) -> {ok, state()}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec list_exchanges(#endpoint{}) -> [{binary(), node(), #exchange{}}].
%% @private
list_exchanges(#endpoint{node = Node, backend = Backend}) ->
    case rpc:call(Node, rabbit_exchange, list, [<<"/">>]) of
        {badrpc, Error} ->
            exit({exchange_rpc_error, Error});
        Exchanges ->
            [{name(E), Backend, declare(E)}
             || E <- Exchanges, not default_exchange(E)]
    end.

-spec default_exchange(#exchange{}) -> true | false.
%% @private
default_exchange(Exchange) ->
    case name(Exchange) of
        <<"amq.", _/binary>>    -> true;
        Bin when size(Bin) == 0 -> true;
        _Other                  -> false
    end.

-spec name(#exchange{}) -> binary().
%% @private
name(#exchange{name = #resource{name = Name}}) -> Name.

-spec declare(#exchange{}) -> #'exchange.declare'{}.
%% @private
declare(#exchange{name        = #resource{name = Name},
                  type        = Type,
                  durable     = Durable,
                  auto_delete = Auto,
                  internal    = Internal,
                  arguments   = Args}) ->
    #'exchange.declare'{exchange   = Name,
                       type        = Type,
                       durable     = Durable,
                       auto_delete = Auto,
                       internal    = Internal,
                       nowait      = false,
                       arguments   = Args}.
