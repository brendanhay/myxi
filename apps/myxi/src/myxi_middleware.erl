%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_middleware).

-include("include/myxi.hrl").

%% API
-export([wrap/3]).

-type result()   :: {method() | unmodified, [action()], [action()]}.
-type composed() :: fun((method()) -> result()).

-export_types([result/0,
               composed/0]).

%%
%% Behaviour
%%

-callback call(#mware{}) -> #mware{}.

%%
%% API
%%

-spec wrap(#endpoint{}, protocol(), [mware()]) -> composed().
%% @doc
wrap(Endpoint, Protocol, MW) ->
    Fn = compose([fun(A) -> P:call(A) end || P <- MW]),
    Args = #mware{endpoint = Endpoint,
                   protocol = Protocol},
    fun(M) -> actions(M, Fn(Args#mware{method = M})) end.

%%
%% Private
%%

-spec compose([fun((#mware{}) -> #mware{})]) -> fun((#mware{}) -> #mware{}).
%% @private
compose(Fns) -> lists:foldl(fun compose/2, fun(X) -> X end, Fns).

-spec compose(fun(), fun()) -> fun().
%% @private
compose(F, G) -> fun(X) -> F(G(X)) end.

-spec actions(method(), #mware{}) -> result().
%% @private
actions(Original, #mware{method = New, pre = Pre, post = Post}) ->
    Status = case New =/= Original of
                 true  -> New;
                 false -> unmodified
             end,
    {Status, Pre, Post}.
