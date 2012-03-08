%% @doc
-module(poxy_policy).

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

-spec thrush(method(), [policy()]) -> {modified(), method()}.
%% @doc
thrush(Method, Policies) ->
    lists:foldl(fun modify/2, {unmodified, Method}, Policies).

%%
%% Private
%%

-spec modify(policy(), {modified(), method()}) -> {modified(), method()}.
%% @private
modify(I, {modified, M}) ->
    {_, N} = I:modify(M),
    {modified, N};
modify(I, {unmodified, M}) ->
    I:modify(M).
