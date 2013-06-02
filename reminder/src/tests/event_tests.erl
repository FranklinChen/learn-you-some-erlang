-module(event_tests).
-include_lib("eunit/include/eunit.hrl").
-test_warnings([start/0, start_link/1, init/0, time_to_go/1]).
%% defined in event.erl
-record(state, {server,
                name="",
                to_go=[0]}).

timeout_test_() ->
    S = self(),
    spawn_link(event, loop, [#state{server=S, name="test", to_go=[2]}]),
    timer:sleep(1000),
    M1 = receive A -> A after 0 -> timeout end,
    timer:sleep(1500),
    M2 = receive B -> B after 0 -> timeout end,
    M3 = receive C -> C after 0 -> timeout end,
    [?_assertEqual(timeout, M1),
     ?_assertEqual({done, "test"}, M2),
     ?_assertEqual(timeout, M3)].

cancel_msg_test_() ->
    S = self(),
    R = make_ref(),
    Pid = spawn_link(event, loop, [#state{server=S, name="test", to_go=[2]}]),
    Pid ! {S, R, cancel},
    M = receive A -> A after 500 -> timeout end,
    [?_assertEqual({R, ok}, M)].

cancel_fn_test_() ->
    S = self(),
    Pid = spawn_link(event, loop, [#state{server=S, name="test", to_go=[2]}]),
    [?_assertEqual(ok, event:cancel(Pid)),
     %% calling cancel again should fail, but still return ok.
     ?_assertEqual(ok, event:cancel(Pid))].

normalize_test_() ->
    [?_assertEqual([0], event:normalize(0)),
     ?_assertEqual([2], event:normalize(2)),
     %% special cases w/ remainders
     ?_assertEqual(1, length(event:normalize(49*24*60*59))),
     ?_assertEqual(2, length(event:normalize(49*24*60*60))),
     ?_assertEqual(2, length(event:normalize(49*24*60*60+1))),
     ?_assertEqual(1000*24*60*60, lists:sum(event:normalize(1000*24*60*60)))].
