%% @doc
-module(poxy_backend_writer).
-behaviour(gen_sock).

%% API
-export([start_link/3,
         forward/2,
         forward/6]).

%% Callbacks
-export([init/4]).

-include("include/poxy.hrl").

-record(s, {sock          :: pid() | undefined,
            intercepts = [] :: intercepts()}).

%%
%% API
%%

-spec start_link(inet:socket(), replay(), intercepts()) -> {ok, pid()}.
%% @doc
start_link(Sock, Replay, Inters) ->
    proc_lib:start_link(?MODULE, init, [self(), Sock, Replay, Inters]).

%% @doc
forward(Writer, Data) ->
    Writer ! {forward, Data},
    receive
        {ok, Writer} ->
            ok
    end.

%% @doc
forward(Writer, RawH, RawP, Channel, Method, Protocol) ->
    Writer ! {forward, self(), [RawH, RawP], Channel, Method, Protocol},
    receive
        {ok, Writer} ->
            ok
    end.

%%
%% Callbacks
%%

-spec init(pid(), inet:socket(), replay(), intercepts()) -> no_return().
%% @hidden
init(Backend, Sock, Replay, Inters) ->
    proc_lib:init_ack(Backend, {ok, self()}),
    replay(#s{sock = Sock, intercepts = Inters}, Replay).

%%
%% Private
%%

-spec replay(inet:socket(), replay()) -> ok.
%% @private
replay(State = #s{sock = Sock}, R = [Payload, Header, Handshake]) ->
    lager:info("REPLAY ~p", [R]),
    ok = send(Sock, Handshake),
    ok = case gen_tcp:recv(Sock, 0) of
             {ok, _Data} -> ok
         end,
    ok = send(Sock, [Header, Payload]),
    loop(State).

-spec loop(#s{}) -> no_return().
%% @private
loop(State = #s{sock = Sock}) ->
    ok = receive
             {forward, From, Data} ->
                 From ! {send(Sock, Data), self()};
             {forward, From, [RawH, RawP], Channel, Method, Protocol} ->
                 From ! {send(Sock, [RawH, RawP]), self()}
         end,
    loop(State).

%%
%% Interception
%%

-spec intercept(iolist(), non_neg_integer(), method(), protocol(), #s{}) -> ok.
%% @private
intercept(Data, 0, _Method, _Protocol, #s{sock = Sock}) ->
    send(Sock, Data);
intercept(Data, _Channel, none, _Protocol, #s{sock = Sock}) ->
    send(Sock, Data);
intercept(Data, Channel, Method, Protocol, #s{sock       = Sock,
                                              intercepts = Inters}) ->
    case poxy_interceptor:thrush(Method, Inters) of
        {modified, NewMethod} ->
            lager:info("MODIFIED ~p", [NewMethod]),
            send(Sock, Channel, NewMethod, Protocol);
        {unmodified, Method} ->
            send(Sock, Data)
    end.

-spec send(inet:socket(), binary()) -> ok.
%% @private
send(Sock, Data) ->
    lager:info("FORWARD ~p", [Data]),
    gen_tcp:send(Sock, Data).

-spec send(inet:socket(), non_neg_integer(), method(), protocol()) -> ok.
%% @private
send(Sock, Channel, Method, Protocol) ->
    Frame =
        rabbit_binary_generator:build_simple_method_frame(Channel,
                                                          Method,
                                                          Protocol),
    send(Sock, Frame).
