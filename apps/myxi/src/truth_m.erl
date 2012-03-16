%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(truth_m).

-behaviour(monad).

%% Callbacks
-export(['>>='/2, return/1, fail/1]).

%%
%% Callbacks
%%

'>>='([], _Fun)    -> false;
'>>='(false, _Fun) -> false;
'>>='(X, Fun)      -> Fun(X).

return([])    -> true;
return(false) -> true;
return(X)     -> X.

fail(_X) -> false.
