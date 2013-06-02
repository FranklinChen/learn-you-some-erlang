-module(cat_fsm_tests).
-include_lib("eunit/include/eunit.hrl").

cat_fsm_test_() ->
    {setup, fun setup/0, fun teardown/1, fun state_test_/1}.

setup() ->
    cat_fsm:start().

teardown(Pid) ->
    exit(Pid, end_test).
    
state_test_(Pid) ->
    [?_assertEqual(dont_give_crap, get_state(Pid)),
     ?_assertEqual({ok, meh}, cat_fsm:event(Pid, event)),
     ?_assertEqual(dont_give_crap, get_state(Pid))].

get_state(Pid) ->
    List = erlang:process_info(Pid),
    {_, {_Mod, Fn, _Arity}} = lists:keyfind(current_function, 1, List),
    Fn.
