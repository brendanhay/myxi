%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin_stats).

-behaviour(gen_server).

-include("include/totochtin.hrl").

%% API
-export([start_link/2,
         connected/1,
         disconnected/2]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type message() :: {connected, pid(), erlang:timestamp()} |
                   {disconnected, pid(),
                    frontend_disconnected | backend_disconnected,
                    erlang:timestamp()}.

-record(s, {sock               :: gen_udp:socket(),
            host = "localhost" :: string(),
            port = 8126        :: inet:port_number(),
            ns = ""            :: string()}).

%%
%% API
%%

-spec start_link(string(), string()) -> ignore | {error, _} | {ok, pid()}.
%% @doc Start the stats process
start_link(Ns, Url) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Ns, Url}, []).

%%
%% Stats
%%

%% @doc
connected(Conn) ->
    cast({connected, Conn, now()}).

%% @doc
disconnected(Conn, Status) ->
    cast({disconnected, Conn, Status, now()}).

%%
%% Callbacks
%%

-spec init({string(), string()}) -> {ok, #s{}}.
%% @hidden
init({Ns, Url}) ->
    lager:info("STATS-INIT"),
    process_flag(trap_exit, true),
    {Host, Port} = totochtin:split_host(Url, 8125),
    {ok, Sock} = gen_udp:open(0, [binary]),
    {ok, #s{sock = Sock, host = Host, port = Port, ns = Ns}}.

-spec handle_call(_, _, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(message(), #s{}) -> {noreply, #s{}} | {stop, normal, #s{}}.
%% listener creates a totochtin_connection
handle_cast({connected, Conn, Started}, State) ->
    gproc:reg(?PROP(Conn), Started),
    stat(State, counter, connect, 1),
    {noreply, State};

%% totochtin_connection terminates
handle_cast({disconnected, Conn, Status, Finished}, State) ->
    Started = gproc:get_value(?PROP(Conn)),
    gproc:unreg(?PROP(Conn)),
    stat(State, timer, Status, now_diff(Finished, Started)),
    stat(State, counter, connect, -1),
    {noreply, State}.

-spec handle_info(_, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

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

-spec cast(message()) -> ok.
%% @private
cast(Msg) -> gen_server:cast(?MODULE, Msg).

-spec now_diff(erlang:timestamp(), erlang:timestamp()) -> non_neg_integer().
%% @private Get the difference in milliseconds between two timestamps
now_diff(T1, T2) -> trunc(timer:now_diff(T1, T2) / 1000).

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
    lager:info("stats: ~p", [Msg]),
    case gen_udp:send(Sock, Host, Port, Msg) of
        _Any -> ok
    end.
