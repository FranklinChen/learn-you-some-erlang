-module(exceptions_tests).
-include_lib("eunit/include/eunit.hrl").

throws_test_() ->
    [?_assertEqual(ok, exceptions:throws(fun() -> a end)),
     ?_assertException(error, {badfun, _} , exceptions:throws(a)),
     ?_assertEqual({throw, caught, a},
                   exceptions:throws(fun() -> throw(a) end))].

errors_test_() ->
    [?_assertEqual(ok, exceptions:errors(fun() -> a end)),
     ?_assertException(throw,
                       a,
                       exceptions:errors(fun() -> throw(a) end)),
     ?_assertEqual({error, caught, a},
                   exceptions:errors(fun() -> erlang:error(a) end))].
exits_test_() ->
    [?_assertEqual(ok, exceptions:exits(fun() -> a end)),
     ?_assertException(error, {badfun, _}, exceptions:exits(a)),
     ?_assertEqual({exit, caught, a},
                   exceptions:exits(fun() -> exit(a) end))].

talk_test() ->
    ?assertEqual("blah blah", exceptions:talk()).

sword_test_() ->
    [?_assertException(throw, slice, exceptions:sword(1)),
     ?_assertException(error, cut_arm, exceptions:sword(2)),
     ?_assertException(exit, cut_leg, exceptions:sword(3)),
     ?_assertException(throw, punch, exceptions:sword(4)),
     ?_assertException(exit, cross_bridge, exceptions:sword(5))].

black_knight_test_() ->
    [?_assertEqual("None shall pass.",
                   exceptions:black_knight(fun exceptions:talk/0)),
     ?_assertEqual("It is but a scratch.",
                   exceptions:black_knight(fun() -> exceptions:sword(1) end)),
     ?_assertEqual("I've had worse.",
                   exceptions:black_knight(fun() -> exceptions:sword(2) end)),
     ?_assertEqual("Come on you pansy!",
                   exceptions:black_knight(fun() -> exceptions:sword(3) end)),
     ?_assertEqual("Just a flesh wound.",
                   exceptions:black_knight(fun() -> exceptions:sword(4) end)),
     ?_assertEqual("Just a flesh wound.",
                   exceptions:black_knight(fun() -> exceptions:sword(5) end))].

whoa_test() ->
    ?assertEqual({caught, throw, up}, exceptions:whoa()).

im_impressed_test() ->
    ?assertEqual({caught, throw, up}, exceptions:im_impressed()).

catcher_test_() ->
    [?_assertEqual("uh oh", exceptions:catcher(1,0)),
     ?_assertEqual(1.0, exceptions:catcher(3,3)),
     ?_assertEqual(2.0, exceptions:catcher(6,3))].

one_or_two_test_() ->
    [?_assertEqual(return, exceptions:one_or_two(1)),
     ?_assertEqual(return, catch exceptions:one_or_two(2)),
     ?_assertException(throw, return, exceptions:one_or_two(2))].
