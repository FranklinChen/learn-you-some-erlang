-module(functions_tests).
-include_lib("eunit/include/eunit.hrl").
-test_warnings([valid_time_test/0]).

head_test() -> ?assertEqual(1, functions:head([1,2,3,4])).

second_test() -> ?assertEqual(2, functions:second([1,2,3,4])).

same_test_() ->
    [?_assertEqual(true, functions:same(a,a)),
     ?_assertEqual(true, functions:same(12,12)),
     ?_assertEqual(false, functions:same(a,b)),
     ?_assertEqual(false, functions:same(12.0, 12))].

%% no clean way to test valid_time's io stuff, so this one is p. much the
%% same thing as the main objective was to test pattern matching.
%% io:format should be used as least as possible to do testing :(
valid_time({_Date = {_Y,_M,_D}, _Time = {_H,_Min,_S}}) ->
    matches;
valid_time(_) ->
    nomatch.

valid_time_test_() ->
    [?_assertEqual(matches, valid_time({{2011,09,06},{09,04,43}})),
     ?_assertEqual(nomatch, valid_time({{2011,09,06},{09,04}}))].


