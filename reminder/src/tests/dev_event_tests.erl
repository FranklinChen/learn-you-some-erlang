-module(dev_event_tests).
-include_lib("eunit/include/eunit.hrl").
%%% Very minimal tests, only verifying that the two implementations of
%%% timeouts work the same in their final and primitive versions
%%% as in the final module. The rest is rather cruft to emulate the normal
%%% event.erl module. Little is tested, because little needs to be tested.


-record(state, {server,
                name="",
                to_go=[0]}).

timeout_test_() ->
    {inorder,
     [fun() -> timeout(loop1, 2) end,
      fun() -> timeout(loop2, [2]) end]}.

timeout(AtomFun, T) ->
    S = self(),
    spawn_link(dev_event, AtomFun, [#state{server=S, name="test", to_go=T}]),
    timer:sleep(1000),
    M1 = receive A -> A after 0 -> timeout end,
    timer:sleep(1500),
    M2 = receive B -> B after 0 -> timeout end,
    M3 = receive C -> C after 0 -> timeout end,
    ?assertEqual(timeout, M1),
    ?assertEqual({done, "test"}, M2),
    ?assertEqual(timeout, M3).

cancel_msg_test_() ->
    {inorder, 
     [fun() -> cancel_msg(loop1, 2) end,
      fun() -> cancel_msg(loop2, [2]) end]}.

cancel_msg(AtomFun, T) ->
    S = self(),
    R = make_ref(),
    Pid = spawn_link(dev_event, AtomFun, [#state{server=S, name="test", to_go=T}]),
    Pid ! {S, R, cancel},
    M = receive A -> A after 500 -> timeout end,
    ?assertEqual({R, ok}, M).
