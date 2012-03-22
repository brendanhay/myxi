%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_listener).

-include("include/myxi_proxy.hrl").

%% API
-export([start_link/0]).

%%
%% API
%%

-spec start_link() -> ok.
%% @doc
start_link() -> lists:foreach(fun listener/1, myxi_config:env(frontends, ?APP)).

%%
%% API
%%

-spec listener(frontend()) -> {ok, pid()}.
%% @private
listener(Config) ->
    Tcp = tcp_options(Config),
    lager:info("LISTEN ~s", [myxi_net:format_ip(Tcp)]),
    cowboy:start_listener(amqp_listener, myxi_config:option(acceptors, Config),
                          cowboy_tcp_transport, Tcp,
                          myxi_connection, Config).

-spec tcp_options(frontend()) -> [proplists:property()].
%% @private
tcp_options(Config) ->
    [{ip, myxi_config:option(ip, Config)},
     {port, myxi_config:option(port, Config)}|myxi_config:env(tcp, ?APP)].
