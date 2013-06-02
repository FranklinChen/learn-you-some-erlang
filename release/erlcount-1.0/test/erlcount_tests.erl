-module(erlcount_tests).
-include_lib("eunit/include/eunit.hrl").
-ifndef(TESTDIR).
%% Assumes we're running from the app's directory. We want to target the
%% 'learn-you-some-erlang' directory.
-define(TESTDIR, "..").
-endif.

%% NOTE:
%% Because we do not want the tests to be bound to a specific snapshot in time
%% of our app, we will instead compare it to an oracle built with unix
%% commands. Users running windows sadly won't be able to run these tests.

%% We'll be forcing the design to be continuation-based when it comes to
%% reading files. This will require some explaining to the user, but will
%% allow to show how we can read files and schedule them at the same time,
%% but without breaking functional principles of referential transparency
%% and while allowing specialised functions to be written in a testable manner.
find_erl_test_() ->
    ?_assertEqual(lists:sort(string:tokens(os:cmd("find "++?TESTDIR++" -name *.erl"), "\n")),
        lists:sort(build_list(erlcount_lib:find_erl(?TESTDIR)))).

build_list(Term) -> build_list(Term, []).

build_list(done, List) -> List;
build_list({continue, Entry, Fun}, List) ->
    build_list(Fun(), [Entry|List]).

regex_count_test_() ->
    [?_assertEqual(5, erlcount_lib:regex_count("a", "a a a a a")),
     ?_assertEqual(0, erlcount_lib:regex_count("o", "a a a a a")),
     ?_assertEqual(2, erlcount_lib:regex_count("a.*", "a a a\na a a")),
     ?_assertEqual(3, erlcount_lib:regex_count("if", "myiffun() ->\n if 1 < \" if \" -> ok;\n    true -> other\n end.\n")),
     ?_assertEqual(1, erlcount_lib:regex_count("if[\\s]{1}(?:.+)->", "myiffun() ->\n if 1 < \" if  \" -> ok;\n    true -> other\n end.\n")),
     ?_assertEqual(2, erlcount_lib:regex_count("if[\\s]{1}(?:.+)->", "myiffun() ->\n if 1 < \" if  \" -> ok;\n    true -> other\n end,\n if true -> ok end.\n"))].
