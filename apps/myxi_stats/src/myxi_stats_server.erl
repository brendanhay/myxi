%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_stats_server).

-behaviour(gen_server).

-include_lib("myxi_lib/include/myxi.hrl").

%% API
-export([start_link/2,
         cast/1,
         call/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type message() :: {connect, pid(), inet:socket(), erlang:timestamp()} |
                   {establish, pid(), node()} |
                   {counter, atom(), pos_integer()}.

-record(s, {sock               :: gen_udp:socket(),
            host = "localhost" :: string(),
            port = 8126        :: inet:port_number(),
            ns = ""            :: string()}).

-record(r, {conn               :: pid(),
            client             :: inet:socket(),
            server = none      :: node() | none,
            started            :: erlang:timestamp()}).

%%
%% API
%%

-spec start_link(string(), string()) -> ignore | {error, _} | {ok, pid()}.
%% @doc Start the stats process
start_link(Ns, Url) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Ns, Url}, []).

-spec cast(message()) -> ok.
%% @doc
cast(Msg) -> gen_server:cast(?MODULE, Msg).

-spec call(message()) -> ok.
%% @doc
call(Msg) -> gen_server:call(?MODULE, Msg).

%%
%% Callbacks
%%

-spec init({string(), string()}) -> {ok, #s{}}.
%% @hidden
init({Ns, Url}) ->
    lager:info("STATS-INIT"),
    process_flag(trap_exit, true),
    {Host, Port} = myxi_util:split_host(Url, 8125),
    {ok, Sock} = gen_udp:open(0, [binary]),
    {ok, #s{sock = Sock, host = Host, port = Port, ns = Ns}}.

-spec handle_call(connections, _, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call(connections, _From, State) ->
    Head = {?PROP('_'), '_', '_'},
    Results = gproc:select([{Head, [], ['$$']}]),
    Conns = [{P, elapsed(T), S} || [?PROP(P), _, #r{server = S, started = T}] <- Results],
    {reply, Conns, State}.

-spec handle_cast(message(), #s{}) -> {noreply, #s{}}.
%% listener creates a myxi_connection
handle_cast({connect, Conn, Client, Started}, State) ->
    true = gproc:reg(?PROP(Conn), #r{client = Client, started = Started}),
    monitor(process, Conn),
    stat(State, counter, connect, 1),
    {noreply, State};

%% connection to backend established
handle_cast({establish, Conn, Server}, State) ->
    Record = gproc:get_value(?PROP(Conn)),
    true = gproc:set_value(?PROP(Conn), Record#r{server = Server}),
    {noreply, State};

%% miscellaneous counter increment
handle_cast({counter, Stat, Step}, State) ->
    stat(State, counter, Stat, Step),
    {noreply, State}.

-spec handle_info({'DOWN', _, process, _, _}, #s{}) -> {noreply, #s{}}.
%% @hidden myxi_connection terminates
handle_info({'DOWN', _Ref, process, Conn, Reason}, State) ->
    Record = gproc:get_value(?PROP(Conn)),
    true = gproc:unreg(?PROP(Conn)),
    Status = case Reason of
                 normal -> disconnect.soft;
                 _Other -> disconnect.hard
             end,
    stat(State, timer, duration, elapsed(Record#r.started)),
    stat(State, counter, Status, 1),
    {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, #s{sock = Sock}) ->
    ok = gen_udp:close(Sock),
    ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec elapsed(erlang:timestamp()) -> non_neg_integer().
%% @private Get the difference in milliseconds now and a timestamp
elapsed(T) -> trunc(timer:now_diff(now(), T) / 1000).

-spec stat(#s{}, counter | timer, string() | atom(), pos_integer(), float()) -> ok.
%% @private Create a statistic entry with a sample rate
stat(State, Type, Bucket, N, Rate) when Rate < 1.0 ->
    case {Type, random:uniform() =< Rate} of
        {counter, true} -> send(State, "~s:~p|c|@~p", [Bucket, N, Rate]);
        {timer, true}   -> send(State, "~s:~p|ms|@~p", [Bucket, N, Rate]);
        _               -> ok
    end.

-spec stat(#s{}, counter | timer, string() | atom(), pos_integer()) -> ok.
%% @doc Create a statistic entry with no sample rate
stat(State, counter, Bucket, N) ->
    send(State, "~s:~p|c", [Bucket, N]);
stat(State, timer, Bucket, N) ->
    send(State, "~s:~p|ms", [Bucket, N]).

-spec send(#s{}, string(), [atom() | non_neg_integer()]) -> ok.
%% @private Send the formatted binary packet over the udp socket,
%% prepending the ns/namespace
send(#s{sock = Sock, host = Host, port = Port, ns = Ns}, Format, Args) ->
    %% iolist_to_bin even though gen_...:send variants accept deep iolists,
    %% since it makes logging and testing easier
    Msg = iolist_to_binary(io_lib:format("~s." ++ Format, [Ns|Args])),
    lager:debug("STAT ~p", [Msg]),
    case gen_udp:send(Sock, Host, Port, Msg) of
        _Any -> ok
    end.
