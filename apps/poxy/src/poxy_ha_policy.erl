%% @doc
-module(poxy_ha_policy).
-behaviour(poxy_policy).

%% Callbacks
-export([modify/1]).

-include("include/poxy.hrl").

-define(KEY, <<"x-ha-policy">>).

%%
%% Callbacks
%%

-spec modify(method()) -> {modified | unmodified, method()}.
%% @doc
modify(Method = #'queue.declare'{arguments = Args}) ->
    NewArgs = lists:keystore(?KEY, 1, Args, {?KEY, longstr, <<"all">>}),
    {modified, Method#'queue.declare'{arguments = NewArgs}};
modify(Method) ->
    {unmodified, Method}.
