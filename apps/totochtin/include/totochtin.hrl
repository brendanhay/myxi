%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%%
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

%%
%% Types
%%

-type version()      :: {0 | 8,0 | 9,0 | 1}.
-type options()      :: [proplists:property()].

-type addr()         :: {inet:ip_address(), inet:port_number()}.

-type policy() :: totochtin_ha_policy.

-type router()       :: totochtin_user_router.

-type balancer()     :: totochtin_round_robin.

-type frontend()     :: [{ip, string()} |
                         {port, pos_integer()} |
                         {max, pos_integer()} |
                         {policys, [policy()]} |
                         {route, router(), [any()]}].

-type backend()      :: [{atom(),
                          {balance, balancer()} |
                          {nodes, [addr()]}}].

-type client()       :: inet:socket().
-type server()       :: inet:socket().

-type user()         :: binary().

-type protocol()     :: rabbit_framing:protocol().
-type method()       :: rabbit_framing:amqp_method_record().

%%
%% Records
%%

-record(policy, {method   :: method | undefined,
                 backend  :: atom(),
                 topology :: ets:tid(),
                 protocol :: protocol()}).

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
