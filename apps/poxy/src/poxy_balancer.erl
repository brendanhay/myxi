%% @doc
-module(poxy_balancer).
-behaviour(gen_server).

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([start_link/3,
         connect/3]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/poxy.hrl").

-type state() :: {module(), [addr()]}.

%%
%% Behaviour
%%

-spec behaviour_info(_) -> [{next, 1}] | undefined.
%% @hidden
behaviour_info(callbacks) -> [{next, 1}];
behaviour_info(_Other)    -> undefined.

%%
%% API
%%

start_link(Name, Mod, Nodes) ->
    lager:info("BALANCE-START ~s ~s", [Name, Mod]),
    gen_server:start_link({local, Name}, ?MODULE, {Mod, Nodes}, []).

connect(Pid, Client, Replay) ->
    Node = gen_server:call(Pid, next),
    {ok, _Pid, Server} = poxy_backend:start_link(Node, Client, Replay),
    Server.

%%
%% Callbacks
%%

init({Mod, Nodes}) ->
    process_flag(trap_exit, true),
    {ok, {Mod, Nodes}}.

-spec handle_call(_, _, state()) -> {reply, ok, state()}.
handle_call(next, _From, {Mod, Nodes}) ->
    {Node, NewNodes} = Mod:next(Nodes),
    lager:info("BALANCE-NEXT ~s ~s", [Mod, poxy:format_addr(Node)]),
    {reply, Node, {Mod, NewNodes}}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
