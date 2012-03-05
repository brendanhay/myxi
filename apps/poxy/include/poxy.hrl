%%
%% Rabbit
%%

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

%%
%% Types
%%

-type version()  :: {0 | 8,0 | 9,0 | 1}.
-type options()  :: [proplists:property()].

-type addr()  :: [{ip, string()} | {port, inet:port_number()}].

-type router()   :: poxy_user_router.

-type balancer() :: poxy_round_robin.

-type frontend() :: [{ip, string()} |
                     {port, pos_integer()} |
                     {max, pos_integer()} |
                     {route, router(), [any()]}].

-type backend()  :: [{atom(),
                      {balance, balancer()} |
                      {nodes, [addr()]}}].

-type client()   :: inet:socket().
-type server()   :: inet:socket().

-type user()     :: binary().
-type replay()   :: iolist().

-type protocol() :: rabbit_framing:protocol().
-type method()   :: rabbit_framing:amqp_method_record().

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
