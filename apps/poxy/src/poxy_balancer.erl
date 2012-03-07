%% @doc
-module(poxy_balancer).
-behaviour(gen_server).

%% Behaviour
-export([behaviour_info/1]).

%% API
-export([start_link/4,
         next/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/poxy.hrl").

-record(s, {mod          :: module(),
            addrs        :: [addr()],
            interceptors :: [interceptor()]}).
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

start_link(Name, Mod, Addrs, Inters) ->
    lager:info("BALANCE-START ~s ~s", [Name, Mod]),
    State = #s{mod = Mod, addrs = Addrs, interceptors = Inters},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

-spec next(pid()) -> {addr(), [interceptor()]}.
%% @doc
next(Pid) -> gen_server:call(Pid, next).

%%
%% Callbacks
%%

init(State) ->
    process_flag(trap_exit, true),
    {ok, State}.

-spec handle_call(next, reference(), #s{}) -> {reply, {addr(), [interceptor()]}, #s{}}.
%% @hidden
handle_call(next, _From, State = #s{mod          = Mod,
                                    addrs        = Addrs,
                                    interceptors = Inters}) ->
    {Addr, NewAddrs} = Mod:next(Addrs),
    lager:info("BALANCE-NEXT ~s ~s", [Mod, poxy:format_addr(Addr)]),
    {reply, {Addr, Inters}, State#s{addrs = NewAddrs}}.

-spec handle_cast(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, #s{}) -> {noreply, #s{}} | {stop, normal, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.
