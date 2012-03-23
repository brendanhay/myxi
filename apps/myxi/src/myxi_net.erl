%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_net).

-include("include/myxi.hrl").

%% API
-export([format_ip/1,
         format_ip/2,
         peername/1,
         hostname/1,
         parse/1,
         parse/2]).

%%
%% API
%%

-spec format_ip([proplists:property()]) -> [byte()].
%% @doc
format_ip({Ip, Port}) ->
    format_ip(Ip, Port);
format_ip(Opts) ->
    format_ip(myxi_config:option(ip, Opts), myxi_config:option(port, Opts)).

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
        {ok, {Ip, Port}} -> format_ip(Ip, Port);
        _Error           -> "DISCONN"
    end.

-spec hostname(node() | atom() | string()) -> inet:hostname().
%% @doc
hostname(Node) when is_atom(Node) ->
    hostname(atom_to_list(Node));
hostname(Node) ->
    [Host|_] = lists:reverse(string:tokens(Node, "@")),
    Host.

-spec parse(string()) -> {nonempty_string(), undefined | inet:port_number()}.
%% @doc
parse(Host) -> parse(Host, undefined).

-spec parse(string(), inet:port_number()) -> {nonempty_string(), inet:port_number()}.
%% @doc
parse(Host, Default) ->
    case string:tokens(Host, ":") of
        [H|P] when length(P) > 0 -> {H, list_to_integer(lists:flatten(P))};
        [H|_]                    -> {H, Default}
    end.
