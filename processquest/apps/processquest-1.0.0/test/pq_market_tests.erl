-module(pq_market_tests).
-include_lib("eunit/include/eunit.hrl").

best_smallest_weapon_test_() ->
    [?_assertMatch({<<"plastic knife">>, 0, 1, 3}, pq_market:weapon(0, 5)),
     ?_assertMatch({<<"plastic knife">>, 1, 1, 5}, pq_market:weapon(1, 100)),
     ?_assertMatch(undefined, pq_market:weapon(0,0)),
     ?_assertMatch(undefined, pq_market:weapon(50000,100000000000000000000))].

best_smallest_gear_test_() ->
    [[?_assertMatch({<<"wool">>, 0, 1, 25}, pq_market:F(0, 35)),
      ?_assertMatch({<<"pleather">>, 0, 2, 45}, pq_market:F(1, 100)),
      ?_assertMatch(undefined, pq_market:F(0,0)),
      ?_assertMatch(undefined, pq_market:F(50000,100000000000000000000))]
        || F <- [helmet, shield, armor]].
