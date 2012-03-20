%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_util).

-include("include/myxi.hrl").

%% API
-export([bin/1,
         config/1,
         option/2,
         format_ip/1,
         format_ip/2,
         peername/1,
         hostname/1,
         split_host/1,
         split_host/2,
         os_env/1,
         os_env/2]).

-define(APPLICATION, myxi).

%%
%% API
%%

-spec bin(atom() | list() | binary()) -> binary().
%% @doc
bin(Atom) when is_atom(Atom)  -> atom_to_binary(Atom, latin1);
bin(List) when is_list(List)  -> list_to_binary(List);
bin(Bin)  when is_binary(Bin) -> Bin.

-spec config(atom()) -> any().
%% @doc
config(Key) ->
    case application:load(?APPLICATION) of
        ok             -> ok;
        {error, Error} -> lager:error("config load ~p", [Error])
    end,
    case application:get_env(?APPLICATION, Key) of
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

-spec format_ip([proplists:property()]) -> [byte()].
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


-spec peername(inet:socket()) -> [byte()].
%% @doc
peername(Sock) ->
    case inet:peername(Sock) of
        {ok, {Ip, Port}} -> myxi_util:format_ip(Ip, Port);
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

-spec os_env(atom()) -> string() | false.
%% @private
os_env(Key) when is_atom(Key) -> os:getenv(atom_to_list(Key)).

-spec os_env(atom() | string(), string()) -> string().
%% @doc
os_env(Key, Default) ->
    case Key of
        K when is_atom(K) ->
            case os_env(K) of
                false -> Default;
                Env   -> Env
            end;
        K when is_list(K) ->
            K
    end.

%%
%% Private
%%

-spec lookup_option(atom(), options()) -> any().
%% @private
lookup_option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.
