-module(sup_tests).
-include_lib("eunit/include/eunit.hrl").

restart_test_() ->
    {"Test that everything restarts until a kill",
     {setup,
      fun() -> sup:start(evserv, []) end,
      fun(_) -> ok end,
      fun restart/1}}.

restart(_SupPid) ->
    timer:sleep(100),
    A = is_pid(whereis(evserv)),
    catch exit(whereis(evserv), die),
    timer:sleep(100),
    B = is_pid(whereis(evserv)),
    catch exit(whereis(evserv), die),
    timer:sleep(100),
    C = is_pid(whereis(evserv)),
    catch exit(whereis(evserv), shutdown),
    timer:sleep(500),
    D = is_pid(whereis(evserv)),
    ?_assertEqual([true,true,true,false],
                  [A,B,C,D]).

