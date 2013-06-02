-module(useless_tests).
-include_lib("eunit/include/eunit.hrl").
-test_warnings([hello_test/0]).

add_test_() ->
    [?_assertEqual(-5, useless:add(-3, -2)),
     ?_assertEqual(4, useless:add(2, 2)),
     ?_assertEqual(2.5, useless:add(2.0, 0.5)),
     ?_assertEqual(1, useless:add(-3, 4))].

hello_test() ->
    ok. % no test possible for I/O. Curse you, side effects!

greet_and_add_test_() ->
    [?_assertEqual(useless:greet_and_add_two(-3), useless:add(-3, 2)),
     ?_assertEqual(useless:greet_and_add_two(2), useless:add(2, 2)),
     ?_assertEqual(useless:greet_and_add_two(0.5), useless:add(2, 0.5)),
     ?_assertEqual(useless:greet_and_add_two(-3), useless:add(-3, 2))].
