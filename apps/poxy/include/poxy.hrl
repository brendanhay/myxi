%%
%% Types
%%

-type version()  :: {0 | 8,0 | 9,0 | 1}.
-type options()  :: [proplists:property()].

-type match() :: {login | match, atom() | binary()}.

-type frontend() :: [{ip, string()} |
                     {port, pos_integer()} |
                     {max, pos_integer()}].

-type backend()  :: [match() |
                     {ip, string()} |
                     {port, pos_integer()}].


-type client() :: inet:socket().
-type server() :: inet:socket().

-type user()     :: binary().
-type replay()   :: iolist().

-type protocol()  :: rabbit_framing:protocol().
-type method()    :: rabbit_framing:amqp_method_record().

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
