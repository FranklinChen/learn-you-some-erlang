-module(dolphins_tests).
-include_lib("eunit/include/eunit.hrl").
%% sorry, this test library is a bit dirty, but it should do
%% the job.

%% cannot test dolphin1/0 for lack of any results outside of I/O.
-test_warnings([dolphin1/0]).

dolphin2_test_() ->
    [?_assertEqual("How about no?", d2_do_a_flip()),
     ?_assertEqual("So long and thanks for all the fish!",
                   d2_fish())].

d2_do_a_flip() ->
    Pid = spawn(dolphins, dolphin2, []),
    Pid ! Pid ! {self(), do_a_flip},
    %% this function receives a message and that's it
    F = fun() ->
        receive
            M -> M
        after 500 ->
            error
        end
    end,
    %% receive the first message
    Msg1 = F(),
    %% only one message should be received. If the second one
    %% is anything except 'error' (no message), return an error.
    %% otherwise send the message for validation.
    case F() of         
        error -> Msg1;  
        _ -> error
    end.
    
d2_fish() ->
    Pid = spawn(dolphins, dolphin2, []),
    Pid ! Pid ! {self(), fish},
    %% this function receives a message and that's it
    F = fun() ->
        receive
            M -> M
        after 500 ->
            error
        end
    end,
    %% receive the first message
    Msg1 = F(),
    %% only one message should be received. If the second one
    %% is anything except 'error' (no message), return an error.
    %% otherwise send the message for validation.
    case F() of         
        error -> Msg1;  
        _ -> error
    end.

dolphin3_test_() ->
    [?_assertEqual(["How about no?",
                    "How about no?",
                    "So long and thanks for all the fish!"],
                    d3())].

d3() ->
    Pid = spawn(dolphins, dolphin3, []),
    Pid ! Pid ! {self(), do_a_flip}, % both should be received
    Pid ! invalid, % should be ignored, but keep the process going
    Pid ! {self(), fish}, % should terminate the process
    Pid ! {self(), do_a_flip}, % should return nothing
    %% this function receives a message and that's it
    F = fun() ->
        receive
            M -> M
        after 500 ->
            error
        end
    end,
    %% receive the expected messages
    Msg1 = F(),
    Msg2 = F(),
    Msg3 = F(),
    Msgs = [Msg1, Msg2, Msg3],
    %% Additional messages should now fail. If a message is
    %% received, add it to the list and let the test fail,
    %% otherwise send the normal message list.
    case F() of         
        error -> Msgs;  
        M -> Msgs ++ [M]
    end.
