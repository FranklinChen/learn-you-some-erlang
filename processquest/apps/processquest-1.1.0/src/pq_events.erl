%%% Wrapper module for the event manager of ProgressQuest players.
%%% It adds a few functions to wrap the events and sleep on the right
%%% scale on behalf of the pq_player process.
-module(pq_events).
-export([killed/3, location/3, lvl_up/5, buy/4, sell/3, quest/4]).
-export([start_link/1, stop/1, add_handler/3, delete_handler/3, notify/2]).

start_link(Name) ->
    {ok, Pid} = gen_event:start_link(),
    ok = regis:register({events, Name}, Pid),
    {ok, Pid}.

stop(Name) ->
    ManagerPid = regis:whereis({events, Name}),
    gen_event:stop(ManagerPid).

add_handler(Name, Handler, Args) ->
    ManagerPid = regis:whereis({events, Name}),
    gen_event:add_handler(ManagerPid, Handler, Args).

delete_handler(Name, Handler, Args) ->  
    ManagerPid = regis:whereis({events, Name}),
    gen_event:delete_handler(ManagerPid, Handler, Args).

notify(Name, Msg) ->
    ManagerPid = regis:whereis({events, Name}),
    gen_event:notify(ManagerPid, Msg).

killed(Name, Enemy = {_EnemyName, _Props}, Time) ->
    notify(Name, {Name, killed, Time*2, Enemy}),
    timer:sleep(Time*2).
    
location(Name, Place, Time) ->
    notify(Name, {Name, heading, Time, Place}), 
    timer:sleep(Time).

lvl_up(Name, NewStats, NewLvl, NewExp, _Time) ->
    notify(Name, {Name, lvl_up, 0, NewStats, NewLvl, NewExp}),
    ok.

buy(Name, Slot, Item, Time) ->
    T = round(Time/2),
    notify(Name, {Name, buy, T, Slot, Item}),
    timer:sleep(T).

sell(Name, Item, Time) ->
    T = round(Time/5),
    notify(Name, {Name, sell, T, Item}),
    timer:sleep(Time).

quest(Name, {Old, _}, {New, _}, _Time) ->
    notify(Name, {Name, quest, 0, Old, New}),
    ok.
