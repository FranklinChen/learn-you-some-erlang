-module(what_the_if_tests).
-include_lib("eunit/include/eunit.hrl").

heh_fine_test() ->
    ?assertException(error, if_clause, what_the_if:heh_fine()).

oh_god_test_() ->
    [?_assertEqual(might_succeed, what_the_if:oh_god(2)),
     ?_assertEqual(always_does, what_the_if:oh_god(3))].

help_me_test_() ->
    [?_assertEqual({cat, "says meow!"}, what_the_if:help_me(cat)),
     ?_assertEqual({beef, "says mooo!"}, what_the_if:help_me(beef)),
     ?_assertEqual({dog, "says bark!"}, what_the_if:help_me(dog)),
     ?_assertEqual({tree, "says bark!"}, what_the_if:help_me(tree)),
     ?_assertEqual({"other", "says fgdadfgna!"}, what_the_if:help_me("other")),
     ?_assertEqual({5, "says fgdadfgna!"}, what_the_if:help_me(5))].
