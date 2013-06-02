-module(regis_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}.

register_test_() ->
    [{"A process can be registered and contacted",
      ?setup(fun register_contact/1)},
     {"A list of registered processes can be obtained",
      ?setup(fun registered_list/1)},
     {"An undefined name should return 'undefined' to crash calls",
      ?setup(fun noregister/1)},
     {"A process can not have two names",
      ?setup(fun two_names_one_pid/1)},
     {"Two processes cannot share the same name",
      ?setup(fun two_pids_one_name/1)}].
      
unregister_test_() ->
    [{"A process that was registered can be registered again iff it was "
      "unregistered between both calls",
      ?setup(fun re_un_register/1)},
     {"Unregistering never crashes",
      ?setup(fun unregister_nocrash/1)},
     {"A crash unregisters a process",
      ?setup(fun crash_unregisters/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, Pid} = regis_server:start_link(),
    Pid.

stop(_) ->
    regis_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(regis_server))].

register_contact(_) ->
    Pid = proc_lib:spawn_link(fun() -> callback(regcontact) end),
    timer:sleep(15),
    Ref = make_ref(),
    WherePid = regis_server:whereis(regcontact),
    regis_server:whereis(regcontact) ! {self(), Ref, hi},
    Rec = receive
         {Ref, hi} -> true
         after 2000 -> false
    end,
    [?_assertEqual(Pid, WherePid),
     ?_assert(Rec)].

noregister(_) ->
    [?_assertError(badarg, regis_server:whereis(make_ref()) ! hi),
     ?_assertEqual(undefined, regis_server:whereis(make_ref()))].

two_names_one_pid(_) ->
    ok = regis_server:register(make_ref(), self()),
    Res = regis_server:register(make_ref(), self()),
    [?_assertEqual({error, already_named}, Res)].

two_pids_one_name(_) ->
    Pid = proc_lib:spawn(fun() -> callback(myname) end),
    timer:sleep(15),
    Res = regis_server:register(myname, self()),    
    exit(Pid, kill),
    [?_assertEqual({error, name_taken}, Res)].

registered_list(_) ->
    L1 = regis_server:get_names(),
    Pids = [spawn(fun() -> callback(N) end) || N <- lists:seq(1,15)],
    timer:sleep(200),
    L2 = regis_server:get_names(),
    [exit(Pid, kill) || Pid <- Pids],
    [?_assertEqual([], L1),
     ?_assertEqual(lists:sort(lists:seq(1,15)), lists:sort(L2))].

re_un_register(_) ->
    Ref = make_ref(),
    L = [regis_server:register(Ref, self()),
         regis_server:register(make_ref(), self()),
         regis_server:unregister(Ref),
         regis_server:register(make_ref(), self())],
    [?_assertEqual([ok, {error, already_named}, ok, ok], L)].

unregister_nocrash(_) ->
    ?_assertEqual(ok, regis_server:unregister(make_ref())).

crash_unregisters(_) ->
    Ref = make_ref(),
    Pid = spawn(fun() -> callback(Ref) end),
    timer:sleep(150),
    Pid = regis_server:whereis(Ref),
    exit(Pid, kill),
    timer:sleep(95),
    regis_server:register(Ref, self()),
    S = regis_server:whereis(Ref),
    Self = self(),
    ?_assertEqual(Self, S).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
callback(Name) ->
    ok = regis_server:register(Name, self()),
    receive
        {From, Ref, Msg} -> From ! {Ref, Msg}
    end.
