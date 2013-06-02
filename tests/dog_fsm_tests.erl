-module(dog_fsm_tests).
-include_lib("eunit/include/eunit.hrl").

bark_test_() ->
    {foreach,
     fun setup_bark/0,
     fun teardown/1,
     [fun bark_pet_test_/1, fun bark_other_test_/1]}.

wag_tail_test_() ->
    {foreach,
     fun setup_wag_tail/0,
     fun teardown/1,
     [fun wag_pet_test_/1, fun wag_other_test_/1]}.

sit_test_() ->
    {foreach,
     fun setup_sit/0,
     fun teardown/1,
     [fun sit_squirrel_test_/1, fun sit_other_test_/1]}.

setup_bark() ->
    Pid = dog_fsm:start(),
    timer:sleep(100),
    Pid.

setup_wag_tail() ->
    Pid = dog_fsm:start(),
    dog_fsm:pet(Pid),
    timer:sleep(100),
    Pid.

setup_sit() ->
    Pid = dog_fsm:start(),
    dog_fsm:pet(Pid),
    dog_fsm:pet(Pid),
    timer:sleep(100),
    Pid.

teardown(Pid) ->
    exit(Pid, end_test).
    
init_test() ->
    Pid = dog_fsm:start(),
    timer:sleep(100),
    ?assertEqual(bark, get_state(Pid)).
    
bark_pet_test_(Pid) ->
    [?_assertEqual(bark, get_state(Pid)),
     ?_assertEqual(pet, dog_fsm:pet(Pid)),
     begin timer:sleep(100), ?_assertEqual(wag_tail, get_state(Pid)) end].

bark_other_test_(Pid) ->
    [?_assertEqual(bark, get_state(Pid)),
     ?_assertEqual(squirrel, dog_fsm:squirrel(Pid)),
     begin timer:sleep(100), ?_assertEqual(bark, get_state(Pid)) end].

wag_pet_test_(Pid) ->
    [?_assertEqual(wag_tail, get_state(Pid)),
     ?_assertEqual(pet, dog_fsm:pet(Pid)),
     begin timer:sleep(100), ?_assertEqual(sit, get_state(Pid)) end].

wag_other_test_(Pid) ->
    [?_assertEqual(wag_tail, get_state(Pid)),
     ?_assertEqual(squirrel, dog_fsm:squirrel(Pid)),
     begin timer:sleep(100), ?_assertEqual(wag_tail, get_state(Pid)) end].

sit_squirrel_test_(Pid) ->
    [?_assertEqual(sit, get_state(Pid)),
     ?_assertEqual(squirrel, dog_fsm:squirrel(Pid)),
     begin timer:sleep(100), ?_assertEqual(bark, get_state(Pid)) end].
    
sit_other_test_(Pid) ->
    [?_assertEqual(sit, get_state(Pid)),
     ?_assertEqual(pet, dog_fsm:pet(Pid)),
     begin timer:sleep(100), ?_assertEqual(sit, get_state(Pid)) end].

get_state(Pid) ->
    List = erlang:process_info(Pid),
    {_, {_Mod, Fn, _Arity}} = lists:keyfind(current_function, 1, List),
    Fn.
