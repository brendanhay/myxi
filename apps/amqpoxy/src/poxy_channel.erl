-module(poxy_channel).

-include("include/amqpoxy.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/3,
         method/2,
         method/3]).

%% Callbacks
-export([init/4]).

-record(v1, {reader,
             sock,
             chan,
             protocol}).

%% Connection tuning
-define(FRAME_MAX_SIZE, 131072).

%%
%% API
%%

start_link(Sock, Conn, Protocol) ->
    proc_lib:start_link(?MODULE, init, [self(), Sock, Conn, Protocol]).

method(Chan, Method) ->
    Chan ! {forward, Method}.

method(Chan, Method, Content) ->
    Chan ! {forward, Method, Content}.

%%
%% Callbacks
%%

%% @hidden
init(Reader, Sock, Conn, Protocol) ->
    {ok, Chan} = open(Conn),
    proc_lib:init_ack(Reader, {ok, self()}),
    ok = reply(Sock, #'channel.open_ok'{}, Protocol),
    loop(#v1{reader = Reader, sock = Sock, chan = Chan, protocol = Protocol}).

%%
%% Private
%%

loop(State = #v1{sock = Sock, chan = Chan, protocol = Protocol}) ->
    receive
        {forward, Method} ->
            ok = forward(Sock, Chan, Method, Protocol);
        {forward, Method, Content} ->
            ok = forward(Sock, Chan, Method, Content, Protocol);
        {Method, Content} ->
            ok
%            ok = rabbit_writer:internal_send_command(Sock, 1, Method, Content, Protocol, ?FRAME_MAX_SIZE)
    end,
    loop(State).

open(Conn) ->
    case amqp_connection:open_channel(Conn) of
        {ok, Chan} ->
            link(Chan),
            {ok, Chan};
        Error ->
            catch amqp_connection:close(Conn),
            throw(Error)
    end.

reply(Sock, Method, Protocol) ->
%    lager:info("REPLY ~s <- ~p", [peername(Sock), Method]),
    ok = rabbit_writer:internal_send_command(Sock, 1, Method, Protocol).

forward(Sock, Chan, Method, Protocol) ->
                                                %    log("METHOD", State),
    case amqp_channel:call(Chan, Method) of
        ok  -> ok;
        Res -> reply(Sock, Res, Protocol)
    end.

forward(Sock, Chan, Method, Content, Protocol) ->
                                                %    log("METHOD-CONTENT", State),
    lager:info("FORWARD-CONTENT ~p", [Method]),
    {Props, Payload} = rabbit_basic:from_content(Content),
    Msg = #amqp_msg{props = Props, payload = Payload},
    case amqp_channel:call(Chan, Method, Msg) of
        ok  -> ok;
        Res ->
            lager:info("RES ~p", [Res]),
            reply(Sock, Res, Protocol)
    end.
