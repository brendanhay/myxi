%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_registry).

-include("include/myxi_proxy.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_balancer/1,
         add_connection/1,
         establish_connection/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type message() :: {connect, pid(), inet:socket(), erlang:timestamp()} |
                   {establish, pid(), #endpoint{}}.

-record(s,    {}).

-record(conn, {client        :: inet:socket(),
               server = none :: #endpoint{} | none,
               started       :: erlang:timestamp()}).


-compile(export_all).

%%
%% GProc Keys
%%

-define(CONN(Pid), ?PROP({connection, Pid})).
-define(BAL,       ?PROP(balancer)).

%%
%% API
%%

-spec start_link() -> {ok, pid()} | {error, _} | ignore.
%% @doc
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% Register
%%

-spec add_balancer(atom()) -> ok.
%% @doc
add_balancer(Name) ->
    %% Register aggregate counter?
    true = gproc:reg(?BAL, Name),
    ok.

-spec add_connection(inet:socket()) -> ok.
%% @doc
add_connection(Client) -> cast({connect, self(), Client, now()}).

-spec establish_connection(#endpoint{}) -> ok.
%% @doc
establish_connection(Endpoint) ->
    %% Register local counter against balancer aggregate?
    cast({establish, self(), Endpoint}).

%%
%% Query
%%

-spec balancers() -> [{pid(), atom()}].
%% @doc
balancers() ->
    Head = {?BAL, '_', '_'},
    Results = gproc:select([{Head, [], ['$$']}]),
    [{P, B} || [?BAL, P, B] <- Results].

-spec connections() -> [{pid(), string(), non_neg_integer(), #endpoint{}}].
%% @doc
connections() ->
    Head = {?CONN('_'), '_', '_'},
    Results = gproc:select([{Head, [], ['$$']}]),
    [{P, myxi_net:peername(C), elapsed(T), S}
     || [?CONN(P), _, #conn{client = C, server = S, started = T}] <- Results].

%%
%% Callbacks
%%

-spec init([]) -> {ok, #s{}}.
%% @hidden
init([]) ->
    lager:info("REGISTRY-INIT"),
    {ok, #s{}}.

-spec handle_call(message(), _, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(message(), #s{}) -> {noreply, #s{}}.
%% listener creates a myxi_connection
handle_cast({connect, Pid, Client, Started}, State) ->
    true = gproc:reg(?CONN(Pid), #conn{client = Client, started = Started}),
    monitor(process, Pid),
    ok = myxi_stats:counter(connect, 1),
    {noreply, State};

%% connection to backend established
handle_cast({establish, Pid, Endpoint}, State) ->
    Conn = gproc:get_value(?CONN(Pid)),
    true = gproc:set_value(?CONN(Pid), Conn#conn{server = Endpoint}),
    {noreply, State}.

-spec handle_info({'DOWN', _, process, _, _}, #s{}) -> {noreply, #s{}}.
%% @hidden myxi_connection terminates
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    Conn = gproc:get_value(?CONN(Pid)),
    true = gproc:unreg(?CONN(Pid)),
    Status = case Reason of
                 normal -> disconnect.soft;
                 _Other -> disconnect.hard
             end,
    myxi_stats:timer(duration, elapsed(Conn#conn.started)),
    myxi_stats:counter(Status, 1),
    {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, _State) -> ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

%% -spec call(message()) -> ok.
%% %% @private
%% call(Msg) -> gen_server:call(?MODULE, Msg).

-spec cast(message()) -> ok.
%% @private
cast(Msg) -> gen_server:cast(?MODULE, Msg).

-spec elapsed(erlang:timestamp()) -> non_neg_integer().
%% @private Get the difference in milliseconds now and a timestamp
elapsed(T) -> trunc(timer:now_diff(now(), T) / 1000).


