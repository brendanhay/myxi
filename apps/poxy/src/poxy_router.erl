%% @doc
-module(poxy_router).

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([new/1,
         route/3]).

-include("include/poxy.hrl").

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{select_balancer, 2}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{select_balancer, 2}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

-spec new(frontend()) -> module().
%% @doc
new(Frontend) ->
    {router, Mod, Routes} = lists:keyfind(router, 1, Frontend),
    Mod:new(Routes).

-spec route(router(), #'connection.start_ok'{}, protocol()) -> addr().
%% @doc
route(Router, StartOk, Protocol) ->
    Balancer = Router:select_balancer(StartOk, Protocol),
    poxy_balancer:next(Balancer).
