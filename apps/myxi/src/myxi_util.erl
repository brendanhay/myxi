%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(myxi_util).

-include("include/myxi.hrl").

%% API
-export([bin/1]).

%%
%% API
%%

-spec bin(atom() | list() | binary()) -> binary().
%% @doc
bin(Atom) when is_atom(Atom)  -> atom_to_binary(Atom, latin1);
bin(List) when is_list(List)  -> list_to_binary(List);
bin(Bin)  when is_binary(Bin) -> Bin.
