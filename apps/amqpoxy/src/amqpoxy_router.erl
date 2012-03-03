%% @doc
-module(amqpoxy_router).

%% API
-export([load/1,
         match/1]).

-include("include/amqpoxy.hrl").

%%
%% API
%%

-spec load([backend()]) -> ok.
%% @doc
load(Backends) ->
    [backend(Opts) || Opts <- Backends],
    ok.

-spec match(match()) -> inet:socket().
%% @doc
match({login, Login}) when is_binary(Login) ->
    Mod = list_to_atom(binary_to_list(Login)),
    case mochiglobal:get(Mod) of
        {Ip, Port} -> connect(Ip, Port);
        undefined  -> error({backend_notfound, Mod})
    end;
match(_Match) ->
    error(match_not_supported).

%%
%% Private
%%

-spec backend(options()) -> ok.
%% @private
backend(Opts) ->
    mochiglobal:put(amqpoxy:option(match, Opts),
                    {amqpoxy:option(ip, Opts), amqpoxy:option(port, Opts)}).

-spec connect(inet:ip_address(), inet:port_number()) -> inet:socket().
%% @private
connect(Ip, Port) ->
    Tcp = [binary, {active, false}, {packet, raw}],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Socket} ->
            Socket;
        Error ->
            lager:error("failed to connect to backend: ~p", [{Ip, Port}]),
            error({backend_unavailable, Ip, Port})
    end.
