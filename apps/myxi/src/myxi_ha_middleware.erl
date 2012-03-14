%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_ha_middleware).

-behaviour(myxi_middleware).

-include("include/myxi.hrl").

%% Callbacks
-export([call/1]).

%%
%% Callbacks
%%

-spec call(#mware{}) -> #mware{}.
%% @doc
call(MW = #mware{method = Method = #'queue.declare'{arguments = Args}}) ->
    MW#mware{method = Method#'queue.declare'{arguments = args(Args)}};
call(MW) ->
    MW.

%%
%% Private
%%

-spec args([tuple()]) -> [tuple()].
%% @private
args(Args) ->
    Merge = [{<<"x-ha-policy">>, longstr, <<"all">>}],
    myxi:merge_keylist(Args, Merge).



