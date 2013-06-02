-module(processquest_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Integration tests verifying the whole app.
-define(setup(Name, T), {setup, fun() -> start(Name) end, fun stop/1, fun T/1}).
-define(setup(T), ?setup(make_ref(), T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
integration_test_() ->
    [{"A player can be started from the processquest module and monitored",
     ?setup(subscribe)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(Name) ->
    application:start(crypto),
    application:start(regis),
    application:start(processquest),
    processquest:start_player(Name, [{time,1100}]),
    Name.

stop(Name) ->
    processquest:stop_player(Name),
    application:stop(processquest),
    application:stop(regis).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
subscribe(Name) ->
    ok = processquest:subscribe(Name, pq_events_handler, self()),
    timer:sleep(4000),
    Msgs = flush(),
    [?_assertMatch([{Name, killed, _Time1, {_EnemyName1, _Props1}},
                    {Name, killed, _Time2, {_EnemyName2, _Props2}},
                    {Name, killed, _Time3, {_EnemyName3, _Props3}}],
                   Msgs)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
flush() ->
    receive
        X -> [X | flush()]
    after 0 -> []
    end.
