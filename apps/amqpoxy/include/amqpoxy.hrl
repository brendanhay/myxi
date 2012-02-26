%%
%% Types
%%

-type backend() :: any().
-type options() :: [proplists:property()].
-type version() :: {0 | 8,0 | 9,0 | 1}.
-type match()   :: {login, atom() | binary()}.

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
