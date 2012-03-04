%% @doc
-module(poxy_auth).

-include("include/poxy.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([decode/1]).

%%
%% API
%%

%% @doc
decode(Response) ->
    case extract_elem(Response) of
        {ok, User, Response1} ->
            case extract_elem(Response1) of
                {ok, _Pass, <<>>} -> User;
                _Error            -> error(error)
            end;
        error ->
            error(error)
    end.

%%
%% Private
%%

%% @private
extract_elem(<<0:8, Rest/binary>>) ->
    Count = next_null_pos(Rest, 0),
    <<Elem:Count/binary, Rest1/binary>> = Rest,
    {ok, Elem, Rest1};
extract_elem(_) ->
    error.

%% @private
next_null_pos(<<>>, Count)                  -> Count;
next_null_pos(<<0:8, _Rest/binary>>, Count) -> Count;
next_null_pos(<<_:8, Rest/binary>>,  Count) -> next_null_pos(Rest, Count + 1).
