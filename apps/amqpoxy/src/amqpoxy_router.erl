%% @doc
-module(amqpoxy_router).
-behaviour(gen_server).

%% API
-export([start_link/1,
         match/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/amqpoxy.hrl").

-record(state, {}).

%%
%% API
%%

-spec start_link([backend()]) -> ignore | {error, _} | {ok, pid()}.
%% @doc
start_link(Backends) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Backends, []).

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
%% Callbacks
%%

-spec init([backend()]) -> {ok, #state{}}.
%% @hidden
init(Backends) ->
    ok = load_backends(Backends),
    {ok, #state{}}.

-spec handle_call(_, _, #state{}) -> {reply, ok, #state{}}.
%% @hidden
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

-spec handle_cast(_, #state{}) -> {noreply, #state{}}.
%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, #state{}) -> {noreply, #state{}} | {stop, normal, #state{}}.
%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, #state{}) -> ok.
%% @hidden
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, #state{}, _) -> {ok, #state{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private
%%

-spec load_backends([backend()]) -> ok.
%% @private
load_backends(Backends) ->
    [update_backend(Opts) || Opts <- Backends],
    ok.

-spec update_backend(options()) -> ok.
%% @private
update_backend(Opts) ->
    mochiglobal:put(amqpoxy:option(match, Opts),
                    {amqpoxy:option(ip, Opts), amqpoxy:option(port, Opts)}).

-spec connect(inet:ip_address(), inet:port_number()) -> inet:socket().
%% @private
connect(Ip, Port) ->
    Tcp = [binary, {active, true}, {packet, raw}, {nodelay, true}],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Socket} ->
            Socket;
        Error ->
            lager:error("failed to connect to backend: ~p", [Error]),
            error({backend_unavailable, Ip, Port})
    end.
