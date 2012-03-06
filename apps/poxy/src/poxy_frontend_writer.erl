%% @doc
-module(poxy_frontend_writer).

%% API
-export([start_link/1,
         reply/2,
         reply/3]).

%% Callbacks
-export([init/2]).

-include("include/poxy.hrl").

%%
%% API
%%

-spec start_link(inet:socket()) -> {ok, pid()}.
%% @doc
start_link(Sock) -> proc_lib:start_link(?MODULE, init, [self(), Sock]).

%% @doc
reply(Writer, Data) ->
    Writer ! {reply, Data},
    ok.

%% @doc
reply(Writer, Method, Protocol) ->
    Writer ! {reply, Method, Protocol},
    ok.

%%
%% Callbacks
%%

-spec init(pid(), inet:socket()) -> no_return().
%% @hidden
init(Frontend, Sock) ->
    proc_lib:init_ack(Frontend, {ok, self()}),
    loop(Sock).

%%
%% Private
%%

%% @private
loop(Sock) ->
    ok = receive
             {reply, Data}             -> send(Sock, Data);
             {reply, Method, Protocol} -> send(Sock, Method, Protocol)
         end,
    loop(Sock).

-spec send(inet:socket(), binary()) -> ok | {error, _}.
%% @private
send(Sock, Data) ->
    lager:info("REPLY ~p", [Data]),
    case poxy:socket_open(Sock) of
        true  -> gen_tcp:send(Sock, Data);
        false -> ok
    end.

-spec send(inet:socket(), method(), protocol()) -> ok.
%% @private
send(Sock, Method, Protocol) ->
    lager:info("REPLY ~p", [Method]),
    case poxy:socket_open(Sock) of
        true ->
            rabbit_writer:internal_send_command(Sock, 0, Method, Protocol);
        false ->
            ok
    end.
