%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_config).

-include("include/myxi.hrl").

%% API
-export([env/1,
         os/1,
         option/2]).

%%
%% API
%%

-spec env(atom()) -> any().
%% @doc
env(Key) ->
    application:load(myxi),
    case application:get_env(myxi, Key) of
        undefined   -> error({config_not_found, Key});
        {ok, Value} -> os(Value)
    end.

-spec os(atom() | string()) -> string().
%% @doc
os(Key) when is_atom(Key) ->
    case os:getenv(atom_to_list(Key)) of
        false -> error({env_not_set, Key});
        Env   -> Env
    end;
os(Key) ->
    Key.

-spec option(ip | atom(), options()) ->  inet:ip_address() | any().
%% @doc
option(ip, Opts) ->
    {ok, Ip} = inet:getaddr(lookup_option(ip, Opts), inet),
    Ip;
option(Key, Opts) ->
    lookup_option(Key, Opts).

%%
%% Private
%%

-spec lookup_option(atom(), options()) -> any().
%% @private
lookup_option(Key, Opts) ->
    {Key, Value} = lists:keyfind(Key, 1, Opts),
    Value.
