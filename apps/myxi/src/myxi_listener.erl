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

-include("include/myxi.hrl").

%% API
-export([start_link/0]).

%%
%% API
%%

-spec start_link() -> ok.
%% @doc
start_link() ->
    [link(P) || {ok, P} <- [listener(C) || C <- myxi_config:env(frontends)]],
    ok.

%%
%% API
%%

-spec listener(frontend()) -> {ok, pid()}.
%% @private
listener(Config) ->
    Tcp = tcp_options(Config),
    Acceptors = myxi_config:option(acceptors, Config),
    case cowboy:start_listener(amqp_listener, Acceptors,
                               cowboy_tcp_transport, Tcp,
                               myxi_connection, Config) of
        {ok, Pid} ->
            lager:info("LISTEN ~s", [myxi_net:format_ip(Tcp)]),
            {ok, Pid};
        Error ->
            lager:error("LISTENER failed to start"),
            exit(listener_start_failure)
    end.

-spec tcp_options(frontend()) -> [proplists:property()].
%% @private
tcp_options(Config) ->
    [{ip, myxi_config:option(ip, Config)},
     {port, myxi_config:option(port, Config)}|myxi_config:env(tcp)].
