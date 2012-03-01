-module(poxy_topology).

-compile(export_all).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

%% %% @doc Determine nodes to route to
%% route(#'connection.start_ok'{response = Res}) ->
%%     Table = rabbit_binary_parser:parse_table(Res),
%%     {value, {_, _, Login}} = lists:keysearch(<<"LOGIN">>, 1, Table),
%%     %% Check the login->nodes index
%%     [].

%% %% @doc Modify queue declarations to be x-ha-policy=all
%% modify(Declare = #'queue.declare'{queue = Name, arguments = Args}) ->
%%     Declare#'queue.declare'{arguments = [{"x-ha-policy", "all"}|Args]};
%% modify(Other) ->
%%     Other.

