%% @doc
-module(poxy_backend_writer).
-behaviour(gen_server).

%% API
-export([start_link/2,
         replay/4,
         forward/2,
         forward/6]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/poxy.hrl").

-record(s, {server          :: pid() | undefined,
            intercepts = [] :: intercepts()}).

%%
%% API
%%

-spec start_link(server(), intercepts()) -> {ok, pid()}.
%% @doc
start_link(Server, Inters) ->
    gen_server:start_link(?MODULE, {Server, Inters}, []).

%% @doc
replay(Writer, Server, Replay, Inters) ->
    gen_server:call(Writer, {replay, Server, Replay, Inters}).

%% @doc
forward(Writer, Data) ->
    gen_server:cast(Writer, {forward, Data}).

%% @doc
forward(Writer, RawH, RawP, Channel, Method, Protocol) ->
    gen_server:cast(Writer, {forward, [RawH, RawP], Channel, Method, Protocol}).

%%
%% Callbacks
%%

-spec init({server(), intercepts()}) -> no_return().
%% @hidden
init({Server, Inters}) ->
    process_flag(trap_exit, true),
    {ok, #s{server = Server, intercepts = Inters}}.

-spec handle_call(_, reference(), #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call({replay, Server, Replay, Inters}, _From, State = #s{server = Server}) ->
    lager:info("writing replay"),
    {reply, server_replay(Server, Replay), State}.

-spec handle_cast(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_cast({forward, Data}, State = #s{server = Server})
  when is_port(Server)->
    ok = server_forward(Server, Data),
    {noreply, State};
handle_cast({forward, [H, P], Channel, Method, Protocol}, State)
  when is_port(State#s.server)->
    ok = server_forward(State#s.server, [H, P]),
    {noreply, State}.

-spec handle_info(_, #s{}) -> {noreply, #s{}} | {stop, normal, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec intercept(iolist(), non_neg_integer(), method(), protocol(), #s{}) -> ok.
%% @private
intercept(Data, 0, _Method, _Protocol, #s{server = Server}) ->
    server_forward(Server, Data);
intercept(Data, _Channel, none, _Protocol, #s{server = Server}) ->
    server_forward(Server, Data);
intercept(Data, Channel, Method, Protocol, #s{server     = Server,
                                              intercepts = Inters}) ->
    case poxy_interceptor:thrush(Method, Inters) of
        {modified, NewMethod} ->
            lager:info("MODIFIED ~p", [NewMethod]),
            server_forward(Server, Channel, NewMethod, Protocol);
        {unmodified, Method} ->
            server_forward(Server, Data)
    end.

%%
%% Sockets
%%

-spec server_replay(server(), replay()) -> ok.
%% @private
server_replay(Server, [Payload, Header, Handshake]) ->
    ok = gen_tcp:send(Server, Handshake),
    ok = case gen_tcp:recv(Server, 0) of
             {ok, _Data} -> ok
         end,
    gen_tcp:send(Server, [Header, Payload]),
    lager:info("REPLAY-OK"),
    ok.

-spec server_forward(server(), binary()) -> ok.
%% @private
server_forward(Server, Data) ->
    lager:info("FORWARD ~p", [Data]),
    gen_tcp:send(Server, Data).

-spec server_forward(server(), non_neg_integer(), method(), protocol()) -> ok.
%% @private
server_forward(Server, Channel, Method, Protocol) ->
    lager:info("FORWARD ~p", [Method]),
    Frame =
        rabbit_binary_generator:build_simple_method_frame(Channel,
                                                          Method,
                                                          Protocol),
    forward(Server, Frame).
