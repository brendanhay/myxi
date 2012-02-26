%%
%% Types
%%

-type version()  :: {0 | 8,0 | 9,0 | 1}.
-type options()  :: [proplists:property()].
-type match()    :: {login | match, atom() | binary()}.

-type frontend() :: [{ip, string()} |
                     {port, pos_integer()} |
                     {max, pos_integer()}].

-type backend()  :: [match() |
                     {ip, string()} |
                     {port, pos_integer()}].

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
