%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(suite_helper).

-compile(export_all).

%%
%% Helpers
%%

send(Pid, Msg) ->
    Pid ! Msg,
    context_switch().

clear([], Config)    -> Config;
clear([H|T], Config) -> clear(T, lists:keydelete(H, 1, Config)).

context_switch() -> erlang:bump_reductions(2000).
