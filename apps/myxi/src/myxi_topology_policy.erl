
%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_topology_policy).

-behaviour(myxi_policy).

-include("include/myxi.hrl").

%% Callbacks
-export([inject/1]).

%%
%% Callbacks
%%

-spec inject(#policy{}) -> #policy{}.
%% @doc
inject(Policy = #policy{method   = #'exchange.declare'{exchange = Name},
                        endpoint = #endpoint{backend = Backend},
                        post     = Post}) ->
    Callback = {apply, myxi_topology, verify_exchange, [Name, Backend]},
    Policy#policy{post = [Callback|Post]};
inject(Policy) ->
    Policy.



