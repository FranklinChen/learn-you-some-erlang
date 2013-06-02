-module(linkmon_tests).
-include_lib("eunit/include/eunit.hrl").

myproc_test_() ->
    {timeout,
     7,
     ?_assertEqual({'EXIT', reason},
                   catch linkmon:myproc())}.

chain_test_() ->
    {timeout,
     3,
     ?_assertEqual(ok, chain_proc())}.

chain_proc() ->
    process_flag(trap_exit, true),
    link(spawn(linkmon, chain, [3])),
    receive
        {'EXIT', _, "chain dies here"} -> ok
    end.

critic1_test_() ->
    Critic = linkmon:start_critic(),
    A = linkmon:judge(Critic, "Genesis", "The Lambda Lies Down on Broadway"),
    exit(Critic, solar_storm),
    B = linkmon:judge(Critic, "Genesis", "A trick of the Tail Recursion"),
    [?_assertEqual("They are terrible!", A),
     ?_assertEqual(timeout, B)].

critic2_test_() ->
    catch unregister(critic),
    linkmon:start_critic2(),
    timer:sleep(200),
    A = linkmon:judge2("The Doors", "Light my Firewall"),
    exit(whereis(critic), kill),
    timer:sleep(200),
    B = linkmon:judge2("Rage Against the Turing Machine", "Unit Testify"),
    exit(whereis(critic), shutdown),
    timer:sleep(200),
    C = (catch linkmon:judge2("a", "b")),
    [?_assertEqual("They are terrible!", A),
     ?_assertEqual("They are great!", B),
     ?_assertMatch({'EXIT', {badarg, _}}, C)].
