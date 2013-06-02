-module(evserv_tests).
-include_lib("eunit/include/eunit.hrl").
-define(FUTURE_DATE, {{2100,1,1},{0,0,0}}).
%% all the tests in this module would be much easier with property-based testing.
%% see Triq, Quvic Quickcheck or Proper for libraries to download that let you
%% do this.
-test_warnings([start/0]).

valid_time_test_() ->
    [?_assert(evserv:valid_time({0,0,0})),
     ?_assert(evserv:valid_time({23,59,59})),
     ?_assert(not evserv:valid_time({23,59,60})),
     ?_assert(not evserv:valid_time({23,60,59})),
     ?_assert(not evserv:valid_time({24,59,59})),
     ?_assert(not evserv:valid_time({-1,0,0})),
     ?_assert(not evserv:valid_time({0,-1,0})),
     ?_assert(not evserv:valid_time({0,0,-1}))].

valid_datetime_test_() ->
    [?_assert(evserv:valid_datetime({{0,1,1},{0,0,0}})),
     ?_assert(evserv:valid_datetime({{2004,2,29},{23,59,59}})),
     ?_assert(evserv:valid_datetime({{2004,12,31},{23,59,59}})),
     ?_assert(not evserv:valid_datetime({{2004,12,31},{23,60,60}})),
     ?_assert(not evserv:valid_datetime({{2003,2,29},{23,59,59}})),
     ?_assert(not evserv:valid_datetime({{0,0,0},{0,0,0}})),
     ?_assert(not evserv:valid_datetime(1209312303))].

loop_test_() ->
    {"Testing all server events on a protocol level",
     [{"Subscribe Tests",
       [{spawn,
         {setup,
          fun evserv:start_link/0,
          fun terminate/1,
          fun subscribe/1}},
        {spawn,
         {setup,
          fun evserv:start_link/0,
          fun terminate/1,
          fun subscribe2/1}},
        {spawn,
         {setup,
          fun evserv:start_link/0,
          fun terminate/1,
          fun down/1}}]},
      {"Adding event integration tests",
       [{spawn,
         {setup,
          fun evserv:start_link/0,
          fun terminate/1,
          fun add/1}},
        {spawn,
         {setup,
          fun evserv:start_link/0,
          fun terminate/1,
          fun addremove/1}},
        {spawn,
         {setup,
          fun evserv:start_link/0,
          fun terminate/1,
          fun done/1}}]}]}.

interface_test_() ->
    {"Testing all server events via the interface functions",
     [{spawn,
       {setup,
        fun evserv:start_link/0,
        fun terminate/1,
        fun interface/1}}]}.

subscribe(Pid) ->
    Ref = make_ref(),
    S = self(),
    Pid ! {S, Ref, {subscribe, S}},
    {ok, M1} = read(),
    Pid ! {S, debug},
    {ok, M2} = read(),
    [?_assertEqual({Ref, ok}, M1),
     ?_assertMatch({state,[], [{_, S}]}, M2)].

subscribe2(Pid) ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    S = self(),
    Pid ! {S, Ref1, {subscribe, S}},
    {ok, M1} = read(),
    Pid ! {S, Ref2, {subscribe, S}},
    {ok, M2} = read(),
    Pid ! {S, debug},
    {ok, M3} = read(),
    [?_assertEqual({Ref1, ok}, M1),
     ?_assertEqual({Ref2, ok}, M2),
     ?_assertMatch({state,[], [{_,S},{_,S}]}, M3)].

down(Pid) ->
    Ref = make_ref(),
    ClientPid = spawn(fun() -> timer:sleep(50000) end),
    Pid ! {self(), debug},
    {ok, S1} = read(),
    Pid ! {self(), Ref, {subscribe, ClientPid}},
    {ok, M1} = read(),
    Pid ! {self(), debug},
    {ok, S2} = read(),
    exit(ClientPid, testkill),
    timer:sleep(100),
    Pid ! {self(), debug},
    {ok, S3} = read(),
    [?_assertMatch({state, _, []}, S1),
     ?_assertEqual({Ref, ok}, M1),
     ?_assertMatch({state, _, [{_,ClientPid}]}, S2),
     ?_assertEqual(S1, S3)].

add(Pid) ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Pid ! {self(), Ref1, {add, "test", "a test event", ?FUTURE_DATE}},
    {ok, M1} = read(),
    Pid ! {self(), Ref2, {add, "test", "a test event2", wrong_date}},
    {ok, M2} = read(),
    [?_assertEqual({Ref1, ok}, M1),
     ?_assertEqual({Ref2, {error, bad_timeout}}, M2)].

addremove(Pid) ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),
    %% ask for useless deletion, check the state
    Pid ! {self(), Ref1, {cancel, "nonexist"}},
    {ok, M1} = read(),
    Pid ! {self(), debug},
    {ok, State1} = read(),
    %% add an event, check the state
    Pid ! {self(), Ref2, {add, "test", "a test event", ?FUTURE_DATE}},
    {ok, M2} = read(),
    Pid ! {self(), debug},
    {ok, State2} = read(),
    %% remove the event, check the state
    Pid ! {self(), Ref3, {cancel, "test"}},
    {ok, M3} = read(),
    Pid ! {self(), debug},
    {ok, State3} = read(),
    [?_assertEqual({Ref1, ok}, M1),
     ?_assertEqual({Ref2, ok}, M2),
     ?_assertEqual({Ref3, ok}, M3),
     ?_assertEqual(State1, State3),
     ?_assert(State2 =/= State3),
     ?_assertMatch({state,[{"test", {event,"test",_,_,?FUTURE_DATE}}],_}, State2)].

done(Pid) ->
    Ref = make_ref(),
    {ok, _} = evserv:subscribe(self()),
    Pid ! {self(), debug},
    {ok, S1} = read(),
    {Date,{H,Min,S}} = calendar:local_time(),
    DateTime = {Date,{H,Min,S+1}},
    Pid ! {self(), Ref, {add, "test", "a test event", DateTime}},
    {ok, M1} = read(),
    Pid ! {self(), debug},
    {ok, S2} = read(),
    X = read(),
    timer:sleep(750),
    {ok, M2} = read(),
    Pid ! {self(), debug},
    {ok, S3} = read(),
    [?_assertMatch({state, [], _}, S1),
     ?_assertEqual({Ref, ok}, M1),
     ?_assertMatch({state, [{"test",_}], _}, S2),
     ?_assertEqual(timeout, X),
     ?_assertEqual({done, "test", "a test event"}, M2),
     ?_assertEqual(S1,S3)].

interface(_Pid) ->
    Ref = evserv:subscribe(self()),
    {Date,{H,Min,S}} = calendar:local_time(),
    M1 = evserv:add_event("test", "desc", {Date,{H,Min,S+1}}),
    M2 = evserv:cancel("test"),
    M3 = evserv:add_event("test1", "desc1", calendar:local_time()),
    M4 = evserv:add_event2("test2", "desc2", calendar:local_time()),
    timer:sleep(100),
    M5 = evserv:listen(2),
    M6 = evserv:add_event("test3", "desc3", some_atom),
    M7 = (catch evserv:add_event2("test4", "desc4", some_atom)),
    [?_assertMatch({ok, _}, Ref),
     ?_assert(is_reference(element(2, Ref))),
     ?_assertEqual(ok, M1),
     ?_assertEqual(ok, M2),
     ?_assertEqual(ok, M3),
     ?_assertEqual(ok, M4),
     ?_assertEqual([{done,"test1","desc1"},{done,"test2","desc2"}], M5),
     ?_assertEqual({error, bad_timeout}, M6),
     ?_assertMatch({'EXIT', {bad_timeout,_}}, M7)].


%% helpers
terminate(Pid) -> Pid ! shutdown.

read() ->
    receive
        M -> {ok, M}
    after 500 ->
        timeout
    end.

