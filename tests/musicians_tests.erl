%% WARNING: THESE TESTS TAKE A LONG TIME TO RUN
-module(musicians_tests).
-include_lib("eunit/include/eunit.hrl").
-define(INSTRUMENTS, [a,b,c,d,e,f,g,h]).

rand_name_test_() ->
    {"Make sure that random names are generated",
     {setup,
      fun setup_many_good/0,
      fun teardown_many/1,
      fun test_names/1}}.

eventual_crash_test_() ->
    {"Checks that bad musicians die at some point, while"
     "good ones don't",
     {inparallel,
      [{timeout, 20000, crash()},
       {timeout, 20000, nocrash()}]}}.

crash() ->
    {ok, Pid} = musicians:start_link(drum, bad),
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    Rec = receive
        {'DOWN', Ref, process, Pid, _R} -> ok
        after 19000 -> timeout
    end,
    ?_assertEqual(ok, Rec).

nocrash() ->
    {ok, Pid} = musicians:start_link(carhorn, good),
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    Rec = receive
        {'DOWN', Ref, process, Pid, _R} -> ok
        after 19000 -> musicians:stop(carhorn), timeout
    end,
    ?_assertEqual(timeout, Rec).

setup_many_good() ->
    [element(2,musicians:start_link(X, good)) ||
        X <- ?INSTRUMENTS].

teardown_many(_) ->
    [musicians:stop(X) || X <- ?INSTRUMENTS].

test_names(Musicians) ->
    Names = [find_name(M) || M <- Musicians],
    SetNames = ordsets:to_list(ordsets:from_list(Names)),
    ?_assert(2 < length(SetNames)). % totally arbitrary ratio

find_name(Inst) ->
    {status, _Pid, _, [_Dict, _Status, _Ancestor, _,
     [_Header, _,
      {data, [{"State", {state, Name, _, _}}]}]]} = sys:get_status(Inst),
    Name.
