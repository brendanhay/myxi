-compile(export_all).

-include("include/poxy.hrl").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EQC(P), ?assert(proper:quickcheck(P))).
