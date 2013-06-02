-module(pq_stats_tests).
-include_lib("eunit/include/eunit.hrl").

all_stats_test_() ->
    Stats = pq_stats:initial_roll(),
    {"Checks whether all stats are returned",
     [?_assertEqual([charisma, constitution, dexterity,
                     intelligence, strength, wisdom],
                    lists:sort(proplists:get_keys(Stats)))]}.

initial_roll_test_() ->
    Rolls = [pq_stats:initial_roll() || _ <- lists:seq(1,100)],
    {"All die rolls are made out of 3 d6 dice",
     %% 6 == number of stats
     [?_assertEqual(6, length([S || {_,S} <- Stats, S >= 3, S =< 18]))
        || Stats <- Rolls]}.

initial_random_roll_test_() ->
    Stats = [pq_stats:initial_roll() || _ <- lists:seq(1,100)],
    {"All die rolls are random",
     ?_assertEqual(lists:sort(Stats),
                   lists:sort(sets:to_list(sets:from_list(Stats))))}.

single_die_roll_test_() ->
    Rolls = [pq_stats:roll() || _ <- lists:seq(1,100)],
    [?_assertEqual(100, length([N || N <- Rolls, N >= 1, N =< 6])),
     ?_assert(1 =/= length(sets:to_list(sets:from_list(Rolls))))].

