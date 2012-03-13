%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_backend).

-include("include/myxi.hrl").

%% API
-export([start_link/3]).

%% Callbacks
-export([init/4]).

-record(s, {connection :: pid(),
            server     :: inet:socket()}).

%%
%% API
%%

-spec start_link(pid(), address(), iolist()) -> {ok, pid()}.
%% @doc
start_link(Conn, Addr, Replay) ->
    proc_lib:start_link(?MODULE, init, [self(), Conn, Addr, Replay]).

%%
%% Callbacks
%%

-spec init(pid(), pid(), inet:socket(), iolist()) -> no_return().
%% @hidden
init(Parent, Conn, Addr, Replay) ->
    Server = connect(Addr, 3),
    ok = gen_tcp:controlling_process(Server, Conn),
    ok = proc_lib:init_ack(Parent, {ok, self(), Server}),
    ok = replay(Server, Replay),
    read(#s{connection = Conn, server = Server}).

%%
%% States
%%

-spec read(#s{}) -> no_return().
%% @private
read(State = #s{connection = Conn, server = Server}) ->
    case gen_tcp:recv(Server, 0) of
        {ok, Data} ->
            myxi_connection:reply(Conn, Data),
            read(State);
        {error, closed} ->
            exit(normal);
        Error ->
            lager:error("BACKEND-ERR ~p", [Error]),
            exit(Error)
    end.
%%
%% Private
%%

-spec replay(inet:socket(), iolist()) -> ok.
%% @doc
replay(Server, [Payload, Header, Handshake]) ->
    ok = gen_tcp:send(Server, Handshake),
    ok = case gen_tcp:recv(Server, 0) of
             {ok, _Data} -> ok
         end,
    gen_tcp:send(Server, [Header, Payload]).

-spec connect(address(), non_neg_integer()) -> inet:socket().
%% @private
connect({Ip, Port}, 0) ->
    exit({backend_timeout, Ip, Port});
connect({Ip, Port}, Retries) ->
    Tcp = [binary, {reuseaddr, true}, {active, false}, {packet, raw}]
        ++ myxi:config(tcp),
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Server} ->
            Server;
        Error ->
            lager:error("BACKEND-ERR ~p", [{Error, Ip, Port}]),
            timer:sleep(500),
            connect({Ip, Port}, Retries - 1)
    end.
