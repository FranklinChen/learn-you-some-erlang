-module(pq_events_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(Name, T), {setup, fun() -> start(Name) end, fun stop/1, fun T/1}).
-define(setup(T), ?setup(make_ref(), T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
events_start_stop_reg_test_() ->
    {"The event handler can be reached, started and stopped by using the "
     "player's name",
      ?setup(can_contact)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(Name) ->
    application:start(regis),
    {ok, Pid} = pq_events:start_link(Name),
    unlink(Pid),
    Name.

stop(Name) ->
    pq_events:stop(Name).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
can_contact(Name) ->
    ok = pq_events:add_handler(Name, pq_events_handler, self()),
    pq_events:notify(Name, hello),
    L1 = flush(),
    pq_events:delete_handler(Name, pq_events_handler, []),
    pq_events:notify(Name, hello),
    L2 = flush(),
    [?_assertEqual([hello], L1),
     ?_assertEqual([], L2)].
    
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
flush() ->
    receive
        X -> [X | flush1()]
    after 300 ->
        []
    end.

flush1() ->
    receive
        X -> [X | flush1()]
    after 0 ->
        []
    end.
