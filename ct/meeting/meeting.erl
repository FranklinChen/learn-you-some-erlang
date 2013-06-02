-module(meeting).
-export([rent_projector/1, use_chairs/1, book_room/1,
        get_all_bookings/0, start/0, stop/0]).
-record(bookings, {projector, room, chairs}).

start() ->
    Pid = spawn(fun() -> loop(#bookings{}) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

rent_projector(Group) ->
    ?MODULE ! {projector, Group}.

book_room(Group) ->
    ?MODULE ! {room, Group}.

use_chairs(Group) ->
    ?MODULE ! {chairs, Group}.

get_all_bookings() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, get_bookings},
    receive
        {Ref, Reply} ->
            Reply
    end.

loop(B = #bookings{}) ->
    receive
        stop -> ok;
        {From, Ref, get_bookings} ->
            From ! {Ref, [{room, B#bookings.room},
                          {chairs, B#bookings.chairs},
                          {projector, B#bookings.projector}]},
            loop(B);
        {room, Group} ->
            loop(B#bookings{room=Group});
        {chairs, Group} ->
            loop(B#bookings{chairs=Group});
        {projector, Group} ->
            loop(B#bookings{projector=Group})
    end.

