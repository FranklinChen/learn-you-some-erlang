-module(calc_tests).
-include_lib("eunit/include/eunit.hrl").

%% runs the unit test function defined in calc.erl
all_test() ->
    ?assert(ok =:= calc:rpn_test()).
