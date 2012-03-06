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

-spec start_link(client()) -> {ok, pid()}.
%% @doc
start_link(Client) -> proc_lib:start_link(?MODULE, init, [self(), Client]).

%% @doc
reply(Writer, Data) -> Writer ! {reply, Data}.

%% @doc
reply(Writer, Method, Protocol) -> Writer ! {reply, Method, Protocol}.

%%
%% Callbacks
%%

-spec init(pid(), client()) -> no_return().
%% @hidden
init(Frontend, Client) ->
    proc_lib:init_ack(Frontend, {ok, self()}),
    loop(Client).

%%
%% Private
%%

%% @private
loop(Client) ->
    ok = receive
             {reply, Data}             -> send(Client, Data);
             {reply, Method, Protocol} -> send(Client, Method, Protocol)
         end,
    loop(Client).

-spec send(client(), binary()) -> ok | {error, _}.
%% @private
send(Client, Data) ->
    lager:info("REPLY ~p", [Data]),
    case poxy:socket_open(Client) of
        true  -> gen_tcp:send(Client, Data);
        false -> ok
    end.

-spec send(client(), method(), protocol()) -> ok.
%% @private
send(Client, Method, Protocol) ->
    lager:info("REPLY ~p", [Method]),
    case poxy:socket_open(Client) of
        true ->
            rabbit_writer:internal_send_command(Client, 0, Method, Protocol);
        false ->
            ok
    end.
