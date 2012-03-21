%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-include_lib("amqp_client/include/amqp_client.hrl").

%%
%% Parse Transforms
%%

%% Logging
-compile({parse_transform, lager_transform}).

%% Currying
-compile({parse_transform, cut}).

%% Monads
-compile({parse_transform, do}).

%%
%% Types
%%

-type version()  :: {0 | 8,0 | 9,0 | 1}.
-type options()  :: [proplists:property()].

-type address()  :: {inet:hostname() | inet:ip_address(), inet:port_number()}.

-type mware()    :: myxi_ha_middleware |
                    myxi_topology_middleware |
                    myxi_federation_middleware.

-type router()   :: myxi_user_router.

-type balancer() :: myxi_round_robin.

-type frontend() :: [{ip, string()} |
                     {port, pos_integer()} |
                     {max, pos_integer()} |
                     {mwares, [mware()]} |
                     {route, router(), [any()]}].

-type backend()  :: [{atom(),
                      {balance, balancer()} |
                      {nodes, [{node(), inet:hostname(), inet:port_number()}]}}].

-type protocol() :: rabbit_framing:protocol().
-type method()   :: rabbit_framing:amqp_method_record().

-type action()   :: {apply, module(), atom(), list()} |
                    {fn, fun(() -> ok)}.

%%
%% Monads
%%

-type error_m(Result, Error) :: ok | {ok, Result} | {error, Error}.
-type truth_m() :: true | false.

%%
%% Records
%%

-record(endpoint, {node         :: node(),
                   backend      :: atom(),
                   address      :: address()}).

-record(mware,    {method       :: method() | undefined,
                   endpoint     :: #endpoint{},
                   protocol     :: protocol(),
                   pre = []     :: [action()],
                   post = []    :: [action()]}).

%%
%% GProc
%%

-define(AGGR(Name), {a, l, {?MODULE, Name}}).
-define(CNTR(Name), {c, l, {?MODULE, Name}}).
-define(PROP(Name), {p, l, {?MODULE, Name}}).

%%
%% Tests
%%

-ifdef(TEST).

-compile(export_all).

-endif.
