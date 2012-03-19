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

-include("include/myxi.hrl").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EQC(P),  ?assert(proper:quickcheck(P))).
-define(_EQC(P), ?_assert(proper:quickcheck(P))).

-compile(export_all).
