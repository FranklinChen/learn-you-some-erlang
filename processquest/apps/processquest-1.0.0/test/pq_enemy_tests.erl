-module(pq_enemy_tests).
-include_lib("eunit/include/eunit.hrl").

is_random_test_() ->
    F = fun(Parent, Ref) -> fun() ->
        <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
        random:seed({A,B,C}),
        Entries = [pq_enemy:fetch() || _ <- lists:seq(1,100)],
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

format_test_() ->
    [[?_assertMatch({_Name, [{drop, {_DropName, _DropVal}},
                             {experience, _Exp}]}, pq_enemy:fetch())
        || _ <- lists:seq(1,10)],
     begin
        {Name, [{drop, {Drop, Val}}, {experience, Exp}]} = pq_enemy:fetch(),
        [?_assert(is_binary(Name)),
         ?_assert(is_binary(Drop)),
         ?_assert(is_integer(Val)),
         ?_assert(is_integer(Exp))]
     end].
