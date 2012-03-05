%% @doc
-module(poxy_user_router, [Routes]).
-behaviour(poxy_router).

%% Callbacks
-export([select_balancer/2]).

-include("include/poxy.hrl").

%%
%% Callbacks
%%

-spec select_balancer(#'connection.start_ok'{}, protocol()) -> atom().
%% @doc
select_balancer(#'connection.start_ok'{response = Response}, Protocol) ->
    User = user(Response, Protocol),
    [Name|_] = [B || [{user, U}, {backend, B}] <- Routes, U =:= User],
    lager:info("ROUTE ~s to ~p", [User, Name]),
    Name.

%%
%% Private
%%

-spec user(binary(), protocol()) -> user().
%% @private
user(Response, rabbit_framing_amqp_0_9_1) ->
    case extract(Response) of
        {ok, User, NewResponse} ->
            case extract(NewResponse) of
                {ok, _Pass, <<>>} -> User;
                _Error            -> error(error)
            end;
        error ->
            error(error)
    end;
user(Response, rabbit_framing_amqp_0_8) ->
    LoginTable = rabbit_binary_parser:parse_table(Response),
    case lists:keysearch(<<"LOGIN">>, 1, LoginTable) of
        {value, {_, longstr, User}} -> User;
        Error                       -> error(Error)
    end.

-spec extract(binary() | any()) -> {ok, binary(), binary()} | error.
%% @private
extract(<<0:8, Rest/binary>>) ->
    Count = null_position(Rest, 0),
    <<Elem:Count/binary, Next/binary>> = Rest,
    {ok, Elem, Next};
extract(_) ->
    error.

-spec null_position(binary(), non_neg_integer()) -> non_neg_integer().
%% @private
null_position(<<>>, Count)                  -> Count;
null_position(<<0:8, _Rest/binary>>, Count) -> Count;
null_position(<<_:8, Rest/binary>>,  Count) -> null_position(Rest, Count + 1).
