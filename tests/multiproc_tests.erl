-module(multiproc_tests).
-include_lib("eunit/include/eunit.hrl").

%% sleep's implementation is copy/pasted from the timer module.
%% not much to test to be safe.
sleep_test_() ->
    [?_assertEqual(ok, multiproc:sleep(10))].

flush_test_() ->
    {spawn,
        [fun() ->
            self() ! a,
            self() ! b,
            ok = multiproc:flush(),
            self() ! c,
            [?assertEqual(receive M -> M end, c)]
         end]}.

priority_test_() ->
    {spawn,
        [fun() ->
            self() ! {15, high},
            self() ! {7, low},
            self() ! {1, low},
            self() ! {17, high},
            [?assertEqual([high, high, low, low],
                          multiproc:important())]
        end]}.
