%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_os).

-include("include/myxi.hrl").

%% API
-export([env/1,
         env/2]).

%%
%% API
%%

-spec env(atom()) -> string() | false.
%% @private
env(Key) when is_atom(Key) -> os:getenv(atom_to_list(Key)).

-spec env(atom() | string(), string()) -> string().
%% @doc
env(Key, Default) ->
    case Key of
        K when is_atom(K) ->
            case env(K) of
                false -> Default;
                Env   -> Env
            end;
        K when is_list(K) ->
            K
    end.
