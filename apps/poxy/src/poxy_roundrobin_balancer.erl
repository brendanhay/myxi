%% @doc
-module(poxy_roundrobin_balancer).
-behaviour(poxy_balancer).

%% Callbacks
-export([next/1]).

-include("include/poxy.hrl").

%%
%% Callbacks
%%

-spec next([addr()]) -> {addr(), [addr()]}.
%% @doc
next([H|T]) -> {H, T ++ [H]}.
