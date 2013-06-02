%% Gives random enemies
-module(pq_enemy).
-export([fetch/0]).

fetch() ->
    L = enemies(),
    lists:nth(random:uniform(length(L)), L).

enemies() ->
    [{<<"Ant">>, [{drop, {<<"Ant Egg">>, 1}}, {experience, 1}]},
     {<<"Wildcat">>, [{drop, {<<"Pelt">>, 1}}, {experience, 1}]},
     {<<"Pig">>, [{drop, {<<"Bacon">>, 1}}, {experience, 1}]},
     {<<"Wild Pig">>, [{drop, {<<"Tasty Ribs">>, 2}}, {experience, 1}]},
     {<<"Goblin">>, [{drop, {<<"Goblin hair">>, 1}}, {experience, 2}]},
     {<<"Robot">>, [{drop, {<<"Chunks of Metal">>, 3}}, {experience, 2}]}].


