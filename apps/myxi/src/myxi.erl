%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(totochtin).

-behaviour(application).

-include("include/totochtin.hrl").

%% API
-export([start/0,
         stop/0,
         config/1,
         option/2,
         format_ip/1,
         format_ip/2,
         peername/1,
         hostname/1,
         split_host/1,
         split_host/2,
         os_env/2]).

%% Callbacks
-export([start/2,
         stop/1]).

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
format_ip({Ip, Port}) ->
    format_ip(Ip, Port);
format_ip(Opts) ->
    format_ip(option(ip, Opts), option(port, Opts)).

-spec format_ip(string() | node() | inet:ip_address(), inet:port_number())
               -> string().
%% @doc
format_ip({A, B, C, D}, Port) ->
    Io = io_lib:fwrite("~p.~p.~p.~p:~p", [A, B, C, D, Port]),
    binary_to_list(iolist_to_binary(Io));
format_ip(Host, Port) ->
    Io = io_lib:fwrite("~s:~p", [Host, Port]),
    binary_to_list(iolist_to_binary(Io)).


-spec peername(inet:socket()) -> string().
%% @doc
peername(Sock) ->
    case inet:peername(Sock) of
        {ok, {Ip, Port}} -> totochtin:format_ip(Ip, Port);
        _Error           -> "DISCONN"
    end.

-spec hostname(node() | atom() | string()) -> inet:hostname().
%% @doc
hostname(Node) when is_atom(Node) ->
    hostname(atom_to_list(Node));
hostname(Node) ->
    [Host|_] = lists:reverse(string:tokens(Node, "@")),
    Host.

-spec split_host(string()) -> {nonempty_string(), undefined | inet:port_number()}.
%% @doc
split_host(Host) -> split_host(Host, undefined).

-spec split_host(string(), T::any()) -> {nonempty_string(), T::any() |
                                        inet:port_number()}.
%% @doc
split_host(Host, Default) ->
    case string:tokens(Host, ":") of
        [H|P] when length(P) > 0 -> {H, list_to_integer(lists:flatten(P))};
        [H|_]                    -> {H, Default}
    end.

-spec os_env(atom() | string(), string()) -> string().
%% @private
os_env(Value, Default) ->
    case Value of
        V when is_atom(V) ->
            case os:getenv(atom_to_list(V)) of
                false -> Default;
                Env -> Env
            end;
        V when is_list(V) ->
            V
    end.

%%
%% Callbacks
%%

-spec start(normal, _) -> ignore | {error, _} | {ok, pid()}.
%% @hidden
start(normal, _Args) ->
    start_listeners(),
    totochtin_sup:start_link().

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
%% Setup
%%

-spec start_listeners() -> [{ok, pid()}].
%% @private
start_listeners() -> [listener(Opts) || Opts <- config(frontends)].

-spec listener(frontend()) -> {ok, pid()}.
%% @private
listener(Config) ->
    Tcp = [{ip, option(ip, Config)},
           {port, option(port, Config)}|config(tcp)],
    lager:info("LISTEN ~s", [format_ip(Tcp)]),
    cowboy:start_listener(amqp_listener, option(max, Config),
                          cowboy_tcp_transport, Tcp,
                          totochtin_connection, Config).

%%
%% Config
%%

-spec lookup_option(atom(), options()) -> any().
%% @private
lookup_option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.
