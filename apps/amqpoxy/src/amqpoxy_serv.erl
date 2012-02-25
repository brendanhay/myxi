-module(amqpoxy_serv).
-behaviour(gen_server).

%% API
-export([start_link/0]).

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

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% Callbacks
%%

-spec init(_) -> {ok, #state{}}.
init(Args) ->
  {ok, Args}.

-spec handle_call(_, _, #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

-spec handle_cast(_, #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(_, #state{}) -> {noreply, #state{}} | {stop, normal, #state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, #state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, #state{}, _) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
