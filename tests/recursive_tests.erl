-module(recursive_tests).
-include_lib("eunit/include/eunit.hrl").

%% those were not in the module, but yeah

fac_test_() ->
    [?_assert(24 == recursive:fac(4)),
     ?_assert(1 == recursive:fac(0)),
     ?_assert(1 == recursive:fac(1)),
     ?_assertError(function_clause, recursive:fac(-1))].

tail_fac_test_() ->
    [?_assert(recursive:fac(4) == recursive:tail_fac(4)),
     ?_assert(recursive:fac(0) == recursive:tail_fac(0)),
     ?_assert(recursive:fac(1) == recursive:tail_fac(1)),
     ?_assertError(function_clause, recursive:tail_fac(-1))].

len_test_() ->
    [?_assert(1 == recursive:len([a])),
     ?_assert(0 == recursive:len([])),
     ?_assert(5 == recursive:len([1,2,3,4,5]))].

tail_len_test_() ->
    [?_assert(recursive:len([a]) == recursive:tail_len([a])),
     ?_assert(recursive:len([]) == recursive:tail_len([])),
     ?_assert(recursive:len([1,2,3,4,5]) == recursive:tail_len([1,2,3,4,5]))].

duplicate_test_() ->
    [?_assert([] == recursive:duplicate(0,a)),
     ?_assert([a] == recursive:duplicate(1,a)),
     ?_assert([a,a,a] == recursive:duplicate(3,a))].

tail_duplicate_test_() ->
    [?_assert(recursive:tail_duplicate(0,a) == recursive:duplicate(0,a)),
     ?_assert(recursive:tail_duplicate(1,a) == recursive:duplicate(1,a)),
     ?_assert(recursive:tail_duplicate(3,a) == recursive:duplicate(3,a))].

reverse_test_() ->
    [?_assert([] == recursive:reverse([])),
     ?_assert([1] == recursive:reverse([1])),
     ?_assert([3,2,1] == recursive:reverse([1,2,3]))].

tail_reverse_test_() ->
    [?_assertEqual(recursive:tail_reverse([]),
                   recursive:reverse([])),
     ?_assertEqual(recursive:tail_reverse([1]),
                   recursive:reverse([1])),
     ?_assertEqual(recursive:tail_reverse([1,2,3]),
                   recursive:reverse([1,2,3]))].

sublist_test_() ->
    [?_assert([] == recursive:sublist([1,2,3],0)),
     ?_assert([1,2] == recursive:sublist([1,2,3],2)),
     ?_assert([] == recursive:sublist([], 4))].

tail_sublist_test_() ->
    [?_assertEqual(recursive:tail_sublist([1,2,3],0),
                   recursive:sublist([1,2,3],0)),
     ?_assertEqual(recursive:tail_sublist([1,2,3],2),
                   recursive:sublist([1,2,3],2)),
     ?_assertEqual(recursive:tail_sublist([], 4),
                   recursive:sublist([], 4))].

zip_test_() ->
    [?_assert([{a,1},{b,2},{c,3}] == recursive:zip([a,b,c],[1,2,3])),
     ?_assert([] == recursive:zip([],[])),
     ?_assertError(function_clause, recursive:zip([1],[1,2]))].

lenient_zip_test_() ->
    [?_assertEqual([{a,1},{b,2},{c,3}],
                   recursive:lenient_zip([a,b,c],[1,2,3])),
     ?_assert([] == recursive:lenient_zip([],[])),
     ?_assert([{a,1}] == recursive:lenient_zip([a],[1,2]))].

%% exercises!
tail_zip_test_() ->
    [?_assertEqual(recursive:tail_zip([a,b,c],[1,2,3]),
                   recursive:zip([a,b,c],[1,2,3])),
     ?_assertEqual(recursive:tail_zip([],[]),
                   recursive:zip([],[])),
     ?_assertError(function_clause, recursive:tail_zip([1],[1,2]))].

tail_lenient_zip_test_() ->
    [?_assertEqual(recursive:tail_lenient_zip([a,b,c],[1,2,3]),
                   recursive:lenient_zip([a,b,c],[1,2,3])),
     ?_assertEqual(recursive:tail_lenient_zip([],[]),
                   recursive:lenient_zip([],[])),
     ?_assertEqual(recursive:tail_lenient_zip([a],[1,2]),
                   recursive:lenient_zip([a],[1,2]))].

%% quick, sort!
quicksort_test_() ->
    [?_assert([] == recursive:quicksort([])),
     ?_assert([1] == recursive:quicksort([1])),
     ?_assert([1,2,2,4,6] == recursive:quicksort([4,2,6,2,1])),
     ?_assert(" JRaceeinqqsuu" == recursive:quicksort("Jacques Requin"))].

lc_quicksort_test_() ->
    [?_assert([] == recursive:lc_quicksort([])),
     ?_assert([1] == recursive:lc_quicksort([1])),
     ?_assert([1,2,2,4,6] == recursive:lc_quicksort([4,2,6,2,1])),
     ?_assert(" JRaceeinqqsuu" == recursive:lc_quicksort("Jacques Requin"))].
    
bestest_qsort_test_() ->
    [?_assert([] == recursive:bestest_qsort([])),
     ?_assert([1] == recursive:bestest_qsort([1])),
     ?_assert([1,2,2,4,6] == recursive:bestest_qsort([4,2,6,2,1])),
     ?_assert(" JRaceeinqqsuu" == recursive:bestest_qsort("Jacques Requin"))].

