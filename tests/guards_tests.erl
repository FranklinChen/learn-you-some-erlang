-module(guards_tests).
-include_lib("eunit/include/eunit.hrl").

old_enough_test_() ->
    [?_assertEqual(true, guards:old_enough(16)),
     ?_assertEqual(false, guards:old_enough(15)),
     ?_assertEqual(false, guards:old_enough(-16))].

right_age_test_() ->
    [?_assertEqual(true, guards:right_age(16)),
     ?_assertEqual(true, guards:right_age(104)),
     ?_assertEqual(true, guards:right_age(50)),
     ?_assertEqual(false, guards:right_age(15)),
     ?_assertEqual(false, guards:right_age(105))].

wrong_age_test_() ->
    [?_assertEqual(false, guards:wrong_age(16)),
     ?_assertEqual(false, guards:wrong_age(104)),
     ?_assertEqual(false, guards:wrong_age(50)),
     ?_assertEqual(true, guards:wrong_age(15)),
     ?_assertEqual(true, guards:wrong_age(105))].

