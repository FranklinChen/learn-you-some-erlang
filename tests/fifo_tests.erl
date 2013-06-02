-module(fifo_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() -> ?assertEqual({fifo,[],[]}, fifo:new()).

push_test_() ->
    [?_assertEqual({fifo,[1,3],[2,4]}, fifo:push({fifo,[3],[2,4]},1)),
     ?_assertEqual({fifo,[2],[]}, fifo:push({fifo,[],[]},2))].

pop_test_() ->
    [?_assertEqual({3,{fifo,[],[2]}}, fifo:pop({fifo,[],[3,2]})),
     ?_assertEqual({3,{fifo,[],[2]}}, fifo:pop({fifo,[2,3],[]})),
     ?_assertEqual({3,{fifo,[2,1],[]}},fifo:pop({fifo,[2,1],[3]})),
     ?_assertEqual({1,{fifo,[],[2,3]}},
                   fifo:pop(fifo:push(fifo:push(fifo:push(fifo:new(),1),2),3))),
     ?_assertError('empty fifo', fifo:pop({fifo,[],[]}))].

empty_test_() ->
    [?_assertEqual(true, fifo:empty(fifo:new())),
     ?_assertEqual(false, fifo:empty({fifo,[1],[]})),
     ?_assertEqual(false, fifo:empty({fifo,[],[1]})),
     ?_assertEqual(false, fifo:empty({fifo,[1],[2]}))].
