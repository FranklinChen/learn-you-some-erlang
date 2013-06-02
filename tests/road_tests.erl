-module(road_tests).
-include_lib("eunit/include/eunit.hrl").
-test_warnings([main/1]).

group_vals_test_() ->
    [?_assertEqual([{a,b,x},{a,b,x}], road:group_vals([a,b,x,a,b,x],[])),
     ?_assertEqual([], road:group_vals([],[])),
     ?_assertError(function_clause, road:group_vals([a,b,x,a],[]))].

parse_map_test_() ->
    [?_assertEqual([], road:parse_map("")),
     ?_assertEqual([], road:parse_map(<<"">>)),
     ?_assertEqual([{10,5,4}], road:parse_map("10 5 4")),
     ?_assertEqual([{10,5,4},{1,2,3}], road:parse_map("10 5 4 1 2 3")),
     ?_assertEqual([{10,5,4}], road:parse_map(<<"10 5 4">>)),
     ?_assertEqual([{10,5,4}], road:parse_map("10\t5\n4")),
     ?_assertEqual([{10,5,4},{1,2,3}],
                   road:parse_map("10\r\n5  4 1\t\t2\r3"))].

%% little testing required on this one, the optimal_path tests will
%% do it in a hidden manner.
shortest_step_test_() ->
    [?_assertEqual({{1,[{a,1}]},{2,[{x,1},{a,1}]}},
                   road:shortest_step({1,8,1},{{0,[]},{0,[]}}))].

optimal_path_test_() ->
    [?_assertEqual([{b,10},{x,30},{a,5},{x,20},{b,2},{b,8}],
                   road:optimal_path(
                     road:parse_map("50 10 30 5 90 20 40 2 24 10 8 0"))),
     ?_assertEqual([{a,1},{a,1},{a,1}],
                   road:optimal_path([{1,10,2},{1,3,3},{1,2,0}])),
     ?_assertEqual([{a,1},{x,1},{b,1},{x,1},{a,1}],
                   road:optimal_path([{1,3,1},{4,1,1},{1,6,1}]))].
