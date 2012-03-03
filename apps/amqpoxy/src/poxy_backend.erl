%% @doc
-module(poxy_backend).

%% API
-compile(export_all).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

%%
%% API
%%

start_link(Client, User, Replay) ->
    proc_lib:start_link(?MODULE, init, [self(), Client, User, Replay]).

%%
%% Callbacks
%%

init(Frontend, Client, User, Replay) ->
    Server = amqpoxy_router:match({login, User}),
    ok = replay(Server, Replay),
    proc_lib:init_ack(Frontend, {ok, self(), Server}),
    loop(Client, Server).

%%
%% Private
%%

-spec connect(inet:ip_address(), inet:port_number()) -> inet:socket().
%% @private
connect(Ip, Port) ->
    lager:info("BACKEND-CONN"),
    Tcp = [binary, {active, false}, {packet, raw}],
    case gen_tcp:connect(Ip, Port, Tcp) of
        {ok, Socket} -> Socket;
        Error        -> error({backend_unavailable, Ip, Port})
    end.

loop(Client, Server) ->
    ok = case gen_tcp:recv(Server, 0) of
             {ok, Data} ->
                 lager:info("BACKEND-RECV"),
                 gen_tcp:send(Client, Data);
             {error, closed} ->
                 exit(normal);
             Error ->
                 exit({backend_error, Error})
         end,
    loop(Client, Server).

replay(Server, [Payload, Header, Handshake]) ->
    lager:info("BACKEND-REPLAY"),
    ok = gen_tcp:send(Server, Handshake),
    ok = case gen_tcp:recv(Server, 0) of
             {ok, _Data} -> ok
         end,
    gen_tcp:send(Server, [Header, Payload]).
