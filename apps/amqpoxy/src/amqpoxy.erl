-module(amqpoxy).
-behaviour(application).

%% API
-export([start/0,
         stop/0,
         config/1]).

%% Callbacks
-export([start/2,
         stop/1]).

-include("include/amqpoxy.hrl").

%%
%% API
%%

-spec start() -> ok.
start() -> start(?MODULE).

-spec stop() -> ok.
stop() ->
    application:stop(?MODULE),
    init:stop().

-spec config(atom()) -> any().
config(Key) ->
    application:load(?MODULE),
    case application:get_env(?MODULE, Key) of
        undefined   -> error({config_not_found, Key});
        {ok, Value} -> Value
    end.

%%
%% Callbacks
%%

-spec start(normal, _) -> ignore | {error, _} | {ok, pid()}.
%% @hidden
start(normal, _Args) -> amqpoxy_sup:start_link().

-spec stop(_) -> ok.
%% @hidden
stop(_State) -> ok.

%%
%% Dependencies
%%

-spec start(atom()) -> ok.
start(App) -> ensure_started(App, application:start(App, permanent)).

-spec ensure_started(atom(), ok | {error, {already_started, atom()} | {not_started, atom()}}) -> ok.
ensure_started(_App, ok) ->
    ok;
ensure_started(_App, {error, {already_started, _App}}) ->
    ok;
ensure_started(App, {error, {not_started, Dep}}) ->
    start(Dep),
    start(App);
ensure_started(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
