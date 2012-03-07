%% @doc
-module(poxy_interceptor).

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([thrush/2]).

-include("include/poxy.hrl").

-type modified() :: modified | unmodified.

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{modify, 1}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{modify, 1}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

-spec thrush(method(), [interceptor()]) -> {modified(), method()}.
%% @doc
thrush(Method, Inters) ->
    lists:foldl(fun modify/2, {unmodified, Method}, Inters).

%%
%% Private
%%

-spec modify(interceptor(), {modified(), method()}) -> {modified(), method()}.
%% @private
modify(I, {modified, M}) ->
    {_, N} = I:modify(M),
    {modified, N};
modify(I, {unmodified, M}) ->
    I:modify(M).
