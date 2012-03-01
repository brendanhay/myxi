-module(poxy_parser).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

%% API
-export([handshake/1,
         header/1,
         payload/3]).

-type(frame_type()   :: ?FRAME_METHOD | ?FRAME_HEADER | ?FRAME_BODY |
                        ?FRAME_OOB_METHOD | ?FRAME_OOB_HEADER | ?FRAME_OOB_BODY |
                        ?FRAME_TRACE | ?FRAME_HEARTBEAT).

-type error()        :: {binary(), any()}.
-type stage()        :: handshake | frame_header | frame_payload | refuse.
-type payload_info() :: {frame_type(), non_neg_integer(), non_neg_integer()}.
-type result()       :: {stage(),
                         #'connection.start'{} |
                         error() |
                         payload_info() |
                         rabbit_framing:amqp_method_record(),
                         rabbit_framing:protocol()}.

%%
%% API
%%

handshake(<<"AMQP", 0, 0, 9, 1>>) ->
    connect({0, 9, 1}, rabbit_framing_amqp_0_9_1);
handshake(<<"AMQP", 1, 1, 0, 9>>) ->
    connect({0, 9, 0}, rabbit_framing_amqp_0_9_1);
handshake(<<"AMQP", 1, 1, 8, 0>>) ->
    connect({8, 0, 0}, rabbit_framing_amqp_0_8);
handshake(<<"AMQP", 1, 1, 9, 1>>) ->
    connect({8, 0, 0}, rabbit_framing_amqp_0_8);
handshake(<<"AMQP", A, B, C, D>>) ->
    refuse({bad_version, A, B, C, D});
handshake(Other) ->
    refuse({bad_handshake, Other}).

header(<<Type:8, Channel:16, Size:32>>) ->
    {info, {Type, Channel, Size}};
header(Data) ->
    refuse({bad_header, Data}).

payload({Type, Channel, Size}, Data, Protocol) ->
    case Data of
        <<Payload:Size/binary, ?FRAME_END>> ->
            unframe(Type, Channel, Payload, Protocol);
        _Unknown ->
            throw({bad_payload, Type, Channel, Size, Data})
    end;
payload(Info, Data, _Protocol) ->
    refuse({bad_payload, Info, Data}).

%%
%% Private
%%

-spec connect({8 | 0, 9 | 0, 1 | 0}, rabbit_framing:protocol()) -> result().
connect({Major, Minor, _Rev}, Protocol) ->
    Start = #'connection.start'{version_major     = Major,
                                version_minor     = Minor,
                                server_properties = properties(Protocol),
                                locales           = <<"en_US">>},
    {start, Start, Protocol}.

-spec properties(rabbit_framing:protocol()) -> rabbit_framing:amqp_table().
properties(Protocol) ->
    [{<<"capabilities">>, table,   capabilities(Protocol)},
     {<<"product">>,      longstr, <<"Poxy">>},
     {<<"version">>,      longstr, <<"0.0.1">>},
     {<<"platform">>,     longstr, <<"Erlang/OTP">>},
     {<<"copyright">>,    longstr, <<"">>},
     {<<"information">>,  longstr, <<"">>}].

-spec capabilities(rabbit_framing:protocol()) -> [{binary(), bool, true}].
capabilities(rabbit_framing_amqp_0_9_1) ->
    [{<<"publisher_confirms">>,         bool, true},
     {<<"exchange_exchange_bindings">>, bool, true},
     {<<"basic.nack">>,                 bool, true},
     {<<"consumer_cancel_notify">>,     bool, true}];
capabilities(_) ->
    [].

-spec refuse(any()) -> result().
refuse(Error) -> {refuse, <<"AMQP", 0, 0, 9, 1>>, Error}.

-spec unframe(frame_type(), non_neg_integer(), binary(),
            rabbit_framing:protocol()) -> result().
%% @doc Channel > 0 indicates a successfully established connection
unframe(Type, _Channel, Payload, Protocol) ->
    case rabbit_command_assembler:analyze_frame(Type, Payload, Protocol) of
        {method, Method, Fields} ->
            {method, Protocol:decode_method_fields(Method, Fields)};
        error ->
            throw({unknown_frame, 0, Type, Payload});
        Other ->
            throw({unexpected_frame, Other})
    end.
