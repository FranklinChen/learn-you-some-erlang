-module(pq_quest_tests).
-include_lib("eunit/include/eunit.hrl").

format_test_() ->
    Quest = pq_quest:fetch(),
    [?_assertMatch({_Name, _Props}, Quest),
     ?_assert(is_binary(element(1,Quest))),
     ?_assert(is_integer(proplists:get_value(kills, element(2,Quest)))),
     ?_assert(0 < proplists:get_value(kills, element(2,Quest))),
     ?_assert(is_integer(proplists:get_value(experience, element(2,Quest)))),
     ?_assert(0 < proplists:get_value(experience, element(2,Quest)))].

is_random_test_() ->
    F = fun(Parent, Ref) -> fun() ->
        <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
        random:seed({A,B,C}),
        Entries = [pq_quest:fetch() || _ <- lists:seq(1,100)],
        Parent ! {Ref, Entries}
    end end,
    Refs = [begin
                Ref = make_ref(),
                spawn_link(F(self(), Ref)),
                Ref
            end || _ <- lists:seq(1,3)],
    [A,B,C] = [receive
                {Ref, X} -> X
               end || Ref <- Refs],
    [?_assert(A =/= B),
     ?_assert(A =/= C),
     ?_assert(B =/= C)].
