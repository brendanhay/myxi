%% @doc
-module(poxy_config).

%% API
-export([listeners/0,
         indexes/0]).

-include("include/poxy.hrl").

listeners() -> config(listeners).

indexes() -> [index(C) || C <- config(clusters)].

index({_Name, Opts}) ->
    Nodes = option(nodes, Opts),
    {index(Nodes, option(exchanges, Opts)),
     index(Nodes, option(queues, Opts))}.

index(Nodes, Items) ->
    [gb_trees:insert(I, Nodes, gb_trees:empty()) || I <- Items].

%%
%% Private
%%

-spec config(atom()) -> any().
%% @private
config(Key) ->
    application:load(?MODULE),
    case application:get_env(?MODULE, Key) of
        undefined   -> error({config_not_found, Key});
        {ok, Value} -> Value
    end.

-spec option(atom(), options()) -> any().
%% @private
option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.
