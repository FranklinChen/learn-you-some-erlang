%%% This module is there to test the incomplete loops and constructs
%%% that are presented in the text, but are not the final result.
-module(dev_event).
-compile(export_all).
-record(state, {server,
                name="",
                to_go=0}).

start1(EventName, Delay) ->
    spawn(?MODULE, init1, [self(), EventName, Delay]).

start_link1(EventName, Delay) ->
    spawn_link(?MODULE, init1, [self(), EventName, Delay]).

start2(EventName, Delay) ->
    spawn(?MODULE, init2, [self(), EventName, Delay]).

start_link2(EventName, Delay) ->
    spawn_link(?MODULE, init2, [self(), EventName, Delay]).

cancel(Pid) ->
    %% Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.



%%% Event's innards
init1(Server, EventName, Delay) ->
    loop2(#state{server=Server,
                name=EventName,
                to_go=normalize(Delay)}).

init2(Server, EventName, DateTime) ->
    loop2(#state{server=Server,
                name=EventName,
                to_go=time_to_go(DateTime)}).

loop1(S = #state{server=Server}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after S#state.to_go * 1000 ->
        Server ! {done, S#state.name}
    end.

%% Loop uses a list for times in order to go around the ~49 days limit
%% on timeouts.
loop2(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T*1000 ->
        if Next =:= [] ->
            Server ! {done, S#state.name};
           Next =/= [] ->
            loop2(S#state{to_go=Next})
        end
    end.


time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
           calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0  -> ToGo;
              ToGo =< 0 -> 0
           end,
    normalize(Secs).

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
