-module(kitchen_tests).
-include_lib("eunit/include/eunit.hrl").

fridge1_test_() ->
    {"Tests fridge1 although the function is never run in the text",
     {foreach,
      fun() -> spawn(kitchen, fridge1, []) end,
      fun(Pid) -> exit(Pid, kill) end,
      [fun fridge1_store/1,
       fun fridge1_take/1]
     }
    }.

fridge1_store(Pid) ->
    Pid ! {self(), {store, item}},
    Reply = receive_or_timeout(),
    [?_assertEqual({Pid, ok}, Reply)].

fridge1_take(Pid) ->
    Pid ! {self(), {take, item}},
    Reply = receive_or_timeout(),
    [?_assertEqual({Pid, not_found}, Reply)].


fridge2_test_() ->
    {"Tests fridge2",
     {foreach,
      fun() -> spawn(kitchen, fridge2, [[]]) end,
      fun(Pid) -> exit(Pid, kill) end,
      [fun fridge2_store/1,
       fun fridge2_take_nostore/1,
       fun fridge2_take_stored/1]
     }
    }.

fridge2_store(Pid) ->
    Pid ! {self(), {store, item}},
    Pid ! {self(), {store, item2}},
    Reply1 = receive_or_timeout(),
    Reply2 = receive_or_timeout(),
    [?_assertEqual({Pid, ok}, Reply1),
     ?_assertEqual({Pid, ok}, Reply2)].

fridge2_take_nostore(Pid) ->
    Pid ! {self(), {take, item}},
    Reply = receive_or_timeout(),
    [?_assertEqual({Pid, not_found}, Reply)].

fridge2_take_stored(Pid) ->
    Pid ! {self(), {store, item}},
    _ = receive_or_timeout(), % flush the 'ok' msg
    Pid ! {self(), {take, item}},
    Pid ! {self(), {take, item}}, % check if the food is removed
    R1 = receive_or_timeout(),
    R2 = receive_or_timeout(),
    [?_assertEqual({Pid, {ok, item}}, R1),
     ?_assertEqual({Pid, not_found}, R2)].


abstractions_test_() ->
    {"Tests the abstraction function we added to the script",
     [{"Basic take/2 and store/2 abstractions. Can't test the hanging.",
       {foreach,
        fun() -> spawn(kitchen, fridge2, [[]]) end,
        fun(Pid) -> exit(Pid, kill) end,
        [fun store/1,
         fun take/1]
       }},
      {"Start/1 abstraction tests. Reuses the take/2 and store/2 tests.",
       {foreach,
        fun() -> kitchen:start([]) end,
        fun(Pid) -> exit(Pid, kill) end,
        [fun store/1,
         fun take/1]
       }},
      {"take2/2 and store2/2 tests.",
       {timeout, 10,
        {foreach,
         fun() -> kitchen:start([]) end,
         fun(Pid) -> exit(Pid, kill) end,
         [fun store2/1,
          fun take2/1]
        }
      }}
     ]
    }.

store(Pid) ->
    [?_assertEqual(ok, kitchen:store(Pid, item))].

take(Pid) ->
    kitchen:store(Pid, item),
    R1 = kitchen:take(Pid, item),
    R2 = kitchen:take(Pid, item),
    [?_assertEqual({ok, item}, R1),
     ?_assertEqual(not_found, R2)].

store2(Pid) ->
    R1 = kitchen:store2(c:pid(0,7,0), item),
    R2 = kitchen:store2(Pid, item),
    [?_assertEqual(timeout, R1),
     ?_assertEqual(ok, R2)].

take2(Pid) ->
    kitchen:store2(Pid, item),
    R1 = kitchen:take2(c:pid(0,7,0), item),
    R2 = kitchen:take2(Pid, item),
    R3 = kitchen:take2(Pid, item),
    [?_assertEqual(timeout, R1),
     ?_assertEqual({ok, item}, R2),
     ?_assertEqual(not_found, R3)].

receive_or_timeout() ->
    receive
        M -> M
    after 1000 ->
        timeout
    end.
