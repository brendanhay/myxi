%%
%% OTP
%%

-type child_spec() :: {atom(),
                       {atom(), atom(), list()},
                        permanent | transient | temporary,
                        brutal_kill | pos_integer() | infinity,
                        worker | supervisor,
                        [atom()]}.

-type supervisor() :: {ok, {
                         {atom(), pos_integer(), pos_integer()}, [child_spec()]}
                      }.

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
