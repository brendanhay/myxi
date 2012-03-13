-module(myxi_SUITE).

-include("myxi_common_test.hrl").

-define(PORT, 5672).

%%
%% Callbacks
%%

init_per_suite(Config) ->
    %% Create a fake broker socket listener
    {ok, Socket} = gen_tcp:listen(?PORT, [binary]),



    Config.


end_per_suite(Config) ->
    [].

all() -> [].

%%
%% Tests
%%




