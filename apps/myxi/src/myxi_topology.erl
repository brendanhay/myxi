%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_topology).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/myxi.hrl").

%% API
-export([start_link/0,
         stop/0,
         add_endpoints/1,
         find_exchange/1,
         verify_exchange/2]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type state() :: atom() | ets:tid().

-record(ex, {name    :: binary() | '$1',
             backend :: atom(),
             declare :: #'exchange.declare'{} | '$2'}).

-define(TABLE, ?MODULE).

%%
%% API
%%

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() -> gen_server:cast(?MODULE, stop).

-spec add_endpoints([#endpoint{}]) -> ok.
%% @doc
add_endpoints([]) ->
    ok;
add_endpoints(Endpoints) ->
    Exchanges = lists:flatten([list_exchanges(E) || E <- Endpoints]),
    lager:info("TOPOLOGY-INS ~p", [Exchanges]),
    true = ets:insert(?TABLE, Exchanges),
    ok.

-spec find_exchange(binary()) -> [{atom(), #'exchange.declare'{}}].
%% @doc
find_exchange(Name) ->
    Matches = ets:match(?TABLE, #ex{name = Name, backend = '$1', declare = '$2'}),
    [{B, D} || [B, D] <- Matches].

-spec verify_exchange(binary(), atom()) -> true | false.
%% @doc
verify_exchange(Name, Backend) ->
    case run_exchange_verification(Name, Backend) of
        ok ->
            true;
        {error, default_exchange} ->
            true;
        {error, Reason} ->
            lager:error("TOPOLOGY-ERR ~p", [Reason]),
            false
    end.

%%
%% Callbacks
%%

-spec init([]) -> {ok, state()}.
%% @hidden
init([]) ->
    lager:info("TOPOLOGY-INIT"),
    process_flag(trap_exit, true),
    {ok, ets:new(?TABLE, [bag, public, named_table, {keypos, #ex.name}])}.

-spec handle_call(info, _, state()) -> {reply, [any()], state()}.
%% @hidden
handle_call(info, _From, State) ->
    {reply, ets:match(?TABLE, '$1'), State}.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.
%% @hidden
handle_cast(stop, State) -> {stop, normal, State}.

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

-spec run_exchange_verification(binary(), atom()) -> error_m(ok, term()).
%% @private
run_exchange_verification(Name, Backend) ->
    do([error_m ||
           case default_exchange(Name) of
               true  -> fail(default_exchange);
               false -> return(valid)
           end,
           {Endpoint, _MW} <- myxi_balancer:next(Backend),
           Exchange        <- locate_exchange(Name, Endpoint),
           add_exchange(Exchange, Backend)]).

-spec locate_exchange(binary(), #endpoint{}) -> error_m(#exchange{}, not_found).
%% @private
locate_exchange(Name, #endpoint{node = Node}) ->
    Resource = rabbit_misc:r(<<"/">>, exchange, Name),
    Args = [{rabbit_exchange, Resource}],
    case rpc:call(Node, rabbit_misc, dirty_read, Args) of
        {badrpc, Error} -> error_m:fail(Error);
        {ok, Result}    -> error_m:return(Result)
    end.

-spec add_exchange(#exchange{}, atom()) -> ok.
%% @private
add_exchange(Exchange, Backend) ->
    true = ets:insert(?TABLE, #ex{name    = name(Exchange),
                                  backend = Backend,
                                  declare = declare(Exchange)}),
    ok.

-spec list_exchanges(#endpoint{}) -> [{binary(), node(), #exchange{}}].
%% @private
list_exchanges(#endpoint{node = Node, backend = Backend}) ->
    case rpc:call(Node, rabbit_exchange, list, [<<"/">>]) of
        {badrpc, _Error} ->
            lager:error("TOPOLOGY-ERR ~s unavailable", [Backend]),
            [];
        Exchanges ->
            [#ex{name = name(E), backend = Backend, declare = declare(E)}
             || E <- Exchanges, not default_exchange(E)]
    end.

-spec default_exchange(#exchange{} | binary()) -> true | false.
%% @private
default_exchange(Exchange = #exchange{})  ->
    default_exchange(name(Exchange));
default_exchange(Name)  ->
    case Name of
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
                       type        = myxi_util:bin(Type),
                       durable     = Durable,
                       auto_delete = Auto,
                       internal    = Internal,
                       nowait      = false,
                       arguments   = Args}.

