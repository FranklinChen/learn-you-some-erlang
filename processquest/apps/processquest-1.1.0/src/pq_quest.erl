-module(pq_quest).
-export([fetch/0]).

fetch() ->
    L = quests(),
    lists:nth(random:uniform(length(L)), L).

quests() ->
    [{<<"Fetch me a nut">>, [{experience, 150}, {kills, 20}]},
     {<<"Cancel the festival">>, [{experience, 65}, {kills, 8}]},
     {<<"Summon the dragon">>, [{experience, 1000}, {kills, 100}]},
     {<<"Meet the invisible man">>, [{experience, 200}, {kills, 25}]},
     {<<"Find quest ideas">>, [{experience, 340}, {kills, 32}]},
     {<<"Invent maple syrup">>, [{experience, 1500}, {kills, 175}]},
     {<<"Slay the Bieber">>, [{experience, 500}, {kills, 45}]}].
