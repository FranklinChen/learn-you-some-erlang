-module(hhfuns_tests).
-include_lib("eunit/include/eunit.hrl").

one_test() ->
    ?assertEqual(1, hhfuns:one()).

two_test() ->
    ?assertEqual(2, hhfuns:two()).

add_test_() ->
    [?_assertEqual(3, hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0)),
     ?_assertError({badfun, _}, hhfuns:add(1,2)),
     ?_assertEqual(2, hhfuns:add(fun hhfuns:one/0, fun hhfuns:one/0))].

increment_test() ->
    ?assertEqual([1,2,3], hhfuns:increment([0,1,2])).

decrement_test() ->
    ?assertEqual([1,2,3], hhfuns:decrement([2,3,4])).

map_test_() ->
    [?_assertEqual([1,2,3], hhfuns:map(fun hhfuns:incr/1, [0,1,2])),
     ?_assertEqual([1,2,3], hhfuns:map(fun hhfuns:decr/1, [2,3,4]))].

bases_test_() ->
    [?_assertEqual(12, hhfuns:base1(3)),
     ?_assertError({badmatch, _}, hhfuns:base2()),
     ?_assertEqual(2, hhfuns:base3())].

closure_test() ->
    ?assertEqual("a/0's password is pony", hhfuns:b(hhfuns:a())).

even_test_() ->
    [?_assertEqual([], hhfuns:even([])),
     ?_assertEqual([], hhfuns:even([3,5,7])),
     ?_assertEqual([2,4], hhfuns:even([1,2,3,4]))].

old_test_() ->
    L = [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}], 
    [?_assertEqual([{male,66},{male,74}], hhfuns:old_men(L)),
     ?_assertEqual([], hhfuns:old_men([{male,45}, {female, -54}])),
     ?_assertEqual([], hhfuns:old_men([]))].
       
filter_test_() ->
    L = [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}], 
    IsEven = fun(X) -> X rem 2 == 0 end,
    IsOldMale = fun({Gender, Age}) -> Gender == male andalso Age > 60 end,
    [?_assertEqual([], hhfuns:filter(IsEven, [])),
     ?_assertEqual([], hhfuns:filter(IsEven, [3,5,7])),
     ?_assertEqual([2,4], hhfuns:filter(IsEven, [1,2,3,4])),
     ?_assertEqual([{male,66},{male,74}], hhfuns:filter(IsOldMale, L)),
     ?_assertEqual([], hhfuns:filter(IsOldMale, [{male,45}, {female, -54}])),
     ?_assertEqual([], hhfuns:filter(IsOldMale, []))].
       
max_test_() ->
    [?_assertEqual(3, hhfuns:max([1,2,3])),
     ?_assertEqual(-1, hhfuns:max([-10,-1,-5.5])),
     ?_assertError(function_clause, hhfuns:max([]))].

min_test_() ->
    [?_assertEqual(0, hhfuns:min([1,2,0,3])),
     ?_assertEqual(-10, hhfuns:min([-10,-1,-5.5])),
     ?_assertError(function_clause, hhfuns:min([]))].

sum_test_() ->
    [?_assertEqual(0, hhfuns:sum([])),
     ?_assertEqual(6, hhfuns:sum([1,2,3]))].

fold_test_() ->
    [H|T] = [1,7,3,5,9,0,2,3],
    [?_assertEqual(9,
                   hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T)),
     ?_assertEqual(0,
                   hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T)),
     ?_assertEqual(21, hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)))].

reverse_test_() ->
    [?_assertEqual([3,2,1], hhfuns:reverse([1,2,3])),
     ?_assertEqual([], hhfuns:reverse([]))].

map2_test_() ->
    [?_assertEqual([1,2,3], hhfuns:map2(fun hhfuns:incr/1, [0,1,2])),
     ?_assertEqual([1,2,3], hhfuns:map2(fun hhfuns:decr/1, [2,3,4]))].


filter2_test_() ->
    L = [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}],
    IsEven = fun(X) -> X rem 2 == 0 end,
    IsOldMale = fun({Gender, Age}) -> Gender == male andalso Age > 60 end,
    [?_assertEqual([], hhfuns:filter2(IsEven, [])),
     ?_assertEqual([], hhfuns:filter2(IsEven, [3,5,7])),
     ?_assertEqual([2,4], hhfuns:filter2(IsEven, [1,2,3,4])),
     ?_assertEqual([{male,66},{male,74}], hhfuns:filter2(IsOldMale, L)),
     ?_assertEqual([], hhfuns:filter2(IsOldMale, [{male,45}, {female, -54}])),
     ?_assertEqual([], hhfuns:filter2(IsOldMale, []))].
