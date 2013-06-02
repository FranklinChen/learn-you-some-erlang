-module(trade_calls).
-compile(export_all).

%% test a little bit of everything and also deadlocks on ready state
%% -- leftover messages possible on race conditions on ready state
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

a(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    trade_fsm:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n",[trade_fsm:ready(Pid)]),
    timer:sleep(1000),
    trade_fsm:make_offer(Pid, "horse"),
    trade_fsm:make_offer(Pid, "sword"),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    trade_fsm:ready(Pid),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(1000).

b(PidA, PidCliA) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    trade_fsm:trade(Pid, PidA),
    trade_fsm:make_offer(Pid, "boots"),
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "boots"),
    timer:sleep(500),
    trade_fsm:make_offer(Pid, "shotgun"),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliA),
    trade_fsm:make_offer(Pid, "horse"), %% race condition!
    trade_fsm:ready(Pid),
    timer:sleep(200),
    timer:sleep(1000).

%% force a race condition on cd trade negotiation
main_cd() ->
    S = self(),
    PidCliC = spawn(fun() -> c(S) end),
    receive PidC -> PidC end,
    spawn(fun() -> d(S, PidC, PidCliC) end),
    receive PidD -> PidD end,
    PidCliC ! PidD.
    
c(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Marc"),
    Parent ! Pid,
    receive PidD -> PidD end,
    io:format("Spawned Marc: ~p~n", [Pid]),
    %sys:trace(Pid, true),
    sync2(),
    trade_fsm:trade(Pid, PidD),
    %% no need to accept_trade thanks to the race condition
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "car"),
    trade_fsm:make_offer(Pid, "horse"),
    timer:sleep(600),
    trade_fsm:cancel(Pid),
    timer:sleep(1000).

d(Parent, PidC, PidCliC) ->
    {ok, Pid} = trade_fsm:start_link("Pete"),
    Parent ! Pid,
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    sync1(PidCliC),
    trade_fsm:trade(Pid, PidC),
    %% no need to accept_trade thanks to the race condition
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "car"),
    trade_fsm:make_offer(Pid, "manatee"),
    timer:sleep(100),
    trade_fsm:ready(Pid),
    timer:sleep(1000).

main_ef() ->
    S = self(),
    PidCliE = spawn(fun() -> e(S) end),
    receive PidE -> PidE end,
    spawn(fun() -> f(PidE, PidCliE) end).

e(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    trade_fsm:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n",[trade_fsm:ready(Pid)]),
    timer:sleep(1000),
    trade_fsm:make_offer(Pid, "horse"),
    trade_fsm:make_offer(Pid, "sword"),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    trade_fsm:ready(Pid),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(1000).

f(PidE, PidCliE) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    trade_fsm:trade(Pid, PidE),
    trade_fsm:make_offer(Pid, "boots"),
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "boots"),
    timer:sleep(500),
    trade_fsm:make_offer(Pid, "shotgun"),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliE),
    trade_fsm:make_offer(Pid, "horse"),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(1000).

%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
