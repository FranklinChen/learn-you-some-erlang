%%% Rolls dice to generate statistics or level increases for a character
-module(pq_stats).
-export([initial_roll/0, roll/0]).

%% First roll, when setting the stats up for the first time
initial_roll() ->
    [{charisma, roll(3)},
     {constitution, roll(3)},
     {dexterity, roll(3)},
     {intelligence, roll(3)},
     {strength, roll(3)},
     {wisdom, roll(3)}].

%% Rolls a single die. Used when leveling up
roll() -> roll(1).

%% Rolls Num 6-faced dice
roll(Num) ->
    lists:sum([random:uniform(6) || _ <- lists:seq(1,Num)]).
