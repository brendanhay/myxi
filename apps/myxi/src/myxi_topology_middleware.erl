%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_topology_middleware).

-behaviour(myxi_middleware).

-include("include/myxi_proxy.hrl").

%% Callbacks
-export([call/1]).

%%
%% Callbacks
%%

-spec call(#mware{}) -> #mware{}.
%% @doc
call(MW = #mware{method = Method, endpoint = Endpoint, post = Post}) ->
    case Method of
        #'exchange.declare'{exchange = Name} ->
            Backend = Endpoint#endpoint.backend,
            Callback = {apply, myxi_topology, verify_exchange, [Name, Backend]},
            MW#mware{post = [Callback|Post]};
        _Other ->
            MW
    end.


