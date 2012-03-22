%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_frontend).

-include_lib("myxi/include/myxi.hrl").

%% API
-export([start_link/2]).

%% Callbacks
-export([init/3]).

-type frame_step()     :: handshake | header | payload.

-type frame_type()     :: ?FRAME_METHOD | ?FRAME_HEADER | ?FRAME_BODY |
                          ?FRAME_OOB_METHOD | ?FRAME_OOB_HEADER | ?FRAME_OOB_BODY |
                          ?FRAME_TRACE | ?FRAME_HEARTBEAT.

-type frame_class_id() :: rabbit_framing:amqp_class_id().

-type frame_state()    :: {method, protocol()} |
                          {content_header, method(), frame_class_id(), protocol()} |
                          {content_body, method(), non_neg_integer(), frame_class_id(), protocol()}.

-type unframed()       :: {method, method()} |
                          {state, frame_state()} |
                          {method, method(), frame_state()} |
                          {method, method(), #content{}, frame_state()}.

-type buffer()         :: [binary()].

-define(HANDSHAKE, 8).
-define(HEADER, 7).
-define(PAYLOAD(Len), Len + 1).

-record(s, {connection            :: pid(),
            client                :: inet:socket(),
            protocol              :: protocol() | undefined,
            step = handshake      :: frame_step(),
            framing               :: frame_state() | undefined,
            payload_info          :: {binary(), integer(), integer(), integer()} | undefined,
            replay = []           :: iolist() | connected,
            buf = []              :: buffer(),
            buf_len = 0           :: non_neg_integer(),
            recv = false          :: boolean(),
            recv_len = ?HANDSHAKE :: non_neg_integer()}).

%%
%% API
%%

-spec start_link(pid(), inet:socket()) -> {ok, pid()}.
%% @doc
start_link(Conn, Client) ->
    proc_lib:start_link(?MODULE, init, [self(), Conn, Client]).

%%
%% Callbacks
%%

-spec init(pid(), pid(), inet:socket()) -> no_return().
%% @hidden
init(Parent, Conn, Client) ->
    lager:debug("FRONTEND-INIT ~p", [Client]),
    proc_lib:init_ack(Parent, {ok, self()}),
    State = #s{connection = Conn, client = Client},
    next_state(State, handshake).

%%
%% Receive
%%

-spec accumulate(#s{}) -> no_return().
%% @private
accumulate(State = #s{recv = true}) ->
    read(State);
accumulate(State = #s{recv_len = RecvLen, buf_len = BufLen}) when BufLen < RecvLen ->
    read(State#s{recv = true});
accumulate(State = #s{recv_len = RecvLen, buf = Buf, buf_len = BufLen}) ->
    {Data, Rest} = split_buffer(Buf, RecvLen),
    NewState = update_replay(Data, State),
    parse(Data, NewState#s{buf = [Rest], buf_len = BufLen - RecvLen}).

-spec read(#s{}) -> no_return().
%% @private
read(State = #s{client = Client, buf = Buf, buf_len = BufLen}) ->
    case gen_tcp:recv(Client, 0) of
        {ok, Data} ->
            accumulate(State#s{buf     = [Data|Buf],
                               buf_len = BufLen + size(Data),
                               recv    = false});
        {error, closed} ->
            disconnect(client_closed, State);
        Error ->
            disconnect(Error, State)
    end.

-spec update_replay(binary(), #s{}) -> #s{}.
%% @private
update_replay(_Data, State = #s{replay = connected}) ->
    State;
update_replay(Data, State = #s{replay = Replay}) ->
    State#s{replay = [Data|Replay]}.

-spec split_buffer(buffer(), pos_integer()) -> {binary(), binary()}.
%% @private
split_buffer(Buf, RecvLen) ->
    split_binary(case Buf of
                     [B]    -> B;
                     _Other -> list_to_binary(lists:reverse(Buf))
                 end,
                 RecvLen).

-spec next_state(#s{}, frame_step()) -> no_return().
%% @private
next_state(State, handshake) ->
    accumulate(State#s{step = handshake, recv_len = ?HANDSHAKE});
next_state(State, header) ->
    accumulate(State#s{step = header, recv_len = ?HEADER});
next_state(State = #s{payload_info = {_Header, _Type, _Channel, Size}}, payload) ->
    accumulate(State#s{step = payload, recv_len = ?PAYLOAD(Size)}).

-spec disconnect(any(), #s{}) -> no_return().
%% @private
disconnect(Error, #s{}) ->
    lager:error("FRONTEND-ERR ~p", [Error]),
    exit(normal).

%%
%% Handshake
%%

-spec connection_start(version(), protocol(), #s{}) -> #s{}.
%% @private
connection_start({Major, Minor, _Rev}, Protocol, State = #s{connection = Conn}) ->
    log("START", State),
    Start = #'connection.start'{version_major     = Major,
                                version_minor     = Minor,
                                server_properties = properties(Protocol),
                                locales           = <<"en_US">>},
    ok = myxi_connection:reply(Conn, 0, Start, Protocol),
    State#s{protocol = Protocol, framing = {method, Protocol}}.

-spec properties(rabbit_framing:protocol()) -> rabbit_framing:amqp_table().
%% @private
properties(Protocol) ->
    [{<<"capabilities">>, table,   capabilities(Protocol)},
     {<<"product">>,      longstr, <<"Myxi">>},
     {<<"version">>,      longstr, <<"0.1.0">>},
     {<<"platform">>,     longstr, <<"Erlang/OTP">>},
     {<<"copyright">>,    longstr, <<"">>},
     {<<"information">>,  longstr, <<"">>}].

-spec capabilities(rabbit_framing:protocol()) -> [{<<_:80,_:_*32>>, bool, true}].
%% @private
capabilities(rabbit_framing_amqp_0_9_1) ->
    [{<<"publisher_confirms">>,         bool, true},
     {<<"exchange_exchange_bindings">>, bool, true},
     {<<"basic.nack">>,                 bool, true},
     {<<"consumer_cancel_notify">>,     bool, true}];
capabilities(_) ->
    [].

-spec connection_start_ok(#'connection.start_ok'{}, #s{}) -> #s{}.
%% @private
connection_start_ok(StartOk, State = #s{connection = Conn,
                                        replay     = Replay,
                                        protocol   = Protocol}) ->
    log("START-OK", State),
    ok = myxi_connection:replay(Conn, StartOk, Replay, Protocol),
    State#s{replay = connected}.

%%
%% Parsing
%%

-spec parse(<<_:8,_:_*8>>, #s{}) -> no_return().
%% @private
parse(Data, State = #s{step = handshake}) ->
    {Version, Protocol} =
        case Data of
            <<"AMQP", 0, 0, 9, 1>> ->
                {{0, 9, 1}, rabbit_framing_amqp_0_9_1};
            <<"AMQP", 1, 1, 0, 9>> ->
                {{0, 9, 0}, rabbit_framing_amqp_0_9_1};
            <<"AMQP", 1, 1, 8, 0>> ->
                {{8, 0, 0}, rabbit_framing_amqp_0_8};
            <<"AMQP", 1, 1, 9, 1>> ->
                {{8, 0, 0}, rabbit_framing_amqp_0_8};
            <<"AMQP", A, B, C, D>> ->
                disconnect({bad_version, A, B, C, D}, State);
            Other ->
                disconnect({bad_handshake, Other}, State)
        end,
    next_state(connection_start(Version, Protocol, State), header);

parse(Data, State = #s{step = header}) ->
    case Data of
        <<Type:8, Channel:16, Size:32>> ->
            next_state(State#s{payload_info = {Data, Type, Channel, Size}}, payload);
        _Other ->
            disconnect({bad_header, Data}, State)
    end;

parse(Data, State = #s{step         = payload,
                       payload_info = {Header, Type, Channel, Size},
                       protocol     = Protocol,
                       framing      = FrameState}) ->
    {NewMethod, NewState} =
        case Data of
            <<Payload:Size/binary, ?FRAME_END>> ->
                case unframe(Type, Channel, Payload, Protocol, FrameState) of
                    {method, StartOk = #'connection.start_ok'{}} ->
                        {ignore, connection_start_ok(StartOk, State)};
                    {method, Method} ->
                        {Method, State};
                    {method, Method, NewFrameState} ->
                        {Method, State#s{framing = NewFrameState}};
                    {method, Method, _Content, NewFrameState} ->
                        {Method, State#s{framing = NewFrameState}};
                    {state, NewFrameState} ->
                        {passthrough, State#s{framing = NewFrameState}}
                end;
            _Unknown ->
                disconnect({bad_payload, Type, Channel, Size, Data}, State)
        end,
    ok = myxi_connection:forward(NewState#s.connection, [Header, Data],
                                 Channel, NewMethod, Protocol),
    next_state(NewState, header).

%%
%% Framing
%%

-spec unframe(frame_type(), non_neg_integer(), binary(), protocol(),
              frame_state()) -> unframed().
%% @private
unframe(Type, 0, Payload, Protocol, _FrameState) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        {method, Method, Fields} ->
            {method, Protocol:decode_method_fields(Method, Fields)};
        heartbeat ->
            heartbeat_not_supported;
        error ->
            {unknown_frame, 0, Type, Payload};
        Unknown ->
            {unknown_frame, 0, Type, Payload, Unknown}
    end;

unframe(Type, Chan, Payload, Protocol, FrameState) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        heartbeat ->
            heartbeat_not_supported;
        error ->
            {unknown_frame, Chan, Type, Payload};
        {method, Method, <<0>>} ->
            {method, Protocol:decode_method_fields(Method, <<0>>)};
        Frame ->
            channel_unframe(Frame, FrameState)
    end.

-spec channel_unframe(any(), frame_state()) -> unframed().
%% @private
channel_unframe(Current, Previous) ->
    case rabbit_command_assembler:process(Current, Previous) of
        {ok, NewFrameState} ->
            {state, NewFrameState};
        {ok, Method, NewFrameState} ->
            {method, Method, NewFrameState};
        {ok, Method, Content, NewFrameState} ->
            {method, Method, Content, NewFrameState};
        Error ->
            Error
    end.

%%
%% Logging
%%

-spec log(string() | atom(), #s{}) -> ok.
%% @private
log(Mode, #s{client = Client}) when is_port(Client) ->
    lager:info("~s ~s -> ~p", [Mode, myxi_util:peername(Client), self()]);
log(Mode, _) ->
    lager:info("~s", [Mode]).
