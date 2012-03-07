%% @doc
-module(poxy_writer).

%% API
-export([send/2,
         send/4,
         replay/2,
         forward/6]).

-include("include/poxy.hrl").

%%
%% API
%%

-spec send(inet:socket(), binary()) -> ok | {error, _}.
%% @private
send(Sock, Data) ->
    case poxy:socket_open(Sock) of
        true  -> gen_tcp:send(Sock, Data);
        false -> ok
    end.

-spec send(inet:socket(), non_neg_integer(), method(), protocol()) -> ok.
%% @doc
send(Sock, 0, Method, Protocol) ->
    rabbit_writer:internal_send_command(Sock, 0, Method, Protocol);
send(Sock, Channel, Method, Protocol) ->
    Frame =
        rabbit_binary_generator:build_simple_method_frame(Channel, Method, Protocol),
    send(Sock, Frame).

-spec replay(inet:socket(), iolist()) -> ok.
%% @doc
replay(Sock, [Payload, Header, Handshake]) ->
    ok = send(Sock, Handshake),
    ok = case gen_tcp:recv(Sock, 0) of
             {ok, _Data} -> ok
         end,
    send(Sock, [Header, Payload]).

%% -spec intercept(iolist(), non_neg_integer(), method(), protocol(), #s{}) -> ok.
%% @doc
forward(_Sock, _Data, _Channel, ignore, _Protocol, _Inters) ->
    ok;
forward(Sock, Data, _Channel, passthrough, _Protocol, _Inters) ->
    send(Sock, Data);
forward(Sock, Data, Channel, Method, Protocol, Inters) ->
    case poxy_interceptor:thrush(Method, Inters) of
        {modified, NewMethod} ->
            lager:info("MODIFIED ~p", [NewMethod]),
            send(Sock, Channel, NewMethod, Protocol);
        {unmodified, Method} ->
            send(Sock, Data)
    end.
