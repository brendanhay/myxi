-module(amqpoxy).
-behaviour(application).

%% API
-export([start/0,
         stop/0,
         config/1,
         option/2,
         format_ip/1,
         format_ip/2]).

%% Callbacks
-export([start/2,
         stop/1]).

-include("include/amqpoxy.hrl").

%%
%% API
%%

-spec start() -> ok.
%% @doc
start() -> start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() ->
    ok = application:stop(?MODULE),
    init:stop().

-spec config(atom()) -> any().
%% @doc
config(Key) ->
    application:load(?MODULE),
    case application:get_env(?MODULE, Key) of
        undefined   -> error({config_not_found, Key});
        {ok, Value} -> Value
    end.

-spec option(ip | atom(), options()) ->  inet:ip_address() | any().
%% @doc
option(ip, Opts) ->
    {ok, Ip} = inet:getaddr(lookup_option(ip, Opts), inet),
    Ip;
option(Key, Opts) ->
    lookup_option(Key, Opts).

-spec format_ip([proplists:property()]) -> string().
%% @doc
format_ip(Opts) ->
    format_ip(option(ip, Opts), option(port, Opts)).

-spec format_ip(inet:ip_address(), inet:port_number()) -> string().
%% @doc
format_ip({A, B, C, D}, Port) ->
    Io = io_lib:fwrite("~p.~p.~p.~p:~p", [A, B, C, D, Port]),
    binary_to_list(iolist_to_binary(Io)).

%%
%% Callbacks
%%

-spec start(normal, _) -> ignore | {error, _} | {ok, pid()}.
%% @hidden
start(normal, _Args) ->
    start_frontends(),
    amqpoxy_sup:start_link(config(backends)).

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

%%
%% Private
%%

-spec lookup_option(atom(), options()) -> any().
%% @private
lookup_option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.

-spec start_frontends() -> [{ok, pid()}].
%% @private
start_frontends() -> [frontend(Opts) || Opts <- config(frontends)].

-spec frontend(options()) -> {ok, pid()}.
%% @private
frontend(Opts) ->
    Tcp = [{ip, option(ip, Opts)}, {port, option(port, Opts)}],
    lager:info("LISTEN ~s", [format_ip(Tcp)]),
    cowboy:start_listener(amqp_listener, option(max, Opts),
                          cowboy_tcp_transport, Tcp,
                          amqpoxy_proxy, Tcp).
