%% Gives random enemies
-module(pq_enemy).
-export([fetch/0]).

fetch() ->
    L = enemies(),
    lists:nth(random:uniform(length(L)), L).

enemies() ->
    [{<<"Spider">>, [{drop, {<<"Spider Egg">>, 1}}, {experience, 1}]},
     {<<"Wildcat">>, [{drop, {<<"Pelt">>, 1}}, {experience, 1}]},
     {<<"Pig">>, [{drop, {<<"Bacon">>, 1}}, {experience, 1}]},
     {<<"Wild Pig">>, [{drop, {<<"Tasty Ribs">>, 2}}, {experience, 1}]},
     {<<"Goblin">>, [{drop, {<<"Goblin hair">>, 1}}, {experience, 2}]},
     {<<"Robot">>, [{drop, {<<"Chunks of Metal">>, 3}}, {experience, 2}]},
     {<<"Factory Worker">>, [{drop, {<<"Wrench">>,2}}, {experience,1}]},
     {<<"Carnie">>, [{drop, {<<"Cotton Candy">>,1}}, {experience,1}]},
     {<<"Mad Beaver">>, [{drop, {<<"Wood chips">>, 2}}, {experience, 1}]},
     {<<"Silent magpie">>, [{drop, {<<"Shiny things">>, 3}}, {experience, 1}]},
     {<<"Great Lizard">>, [{drop, {<<"Lizard tail">>, 1}}, {experience, 1}]},
     {<<"Cheetah">>, [{drop, {<<"Fur">>, 3}}, {experience, 4}]},
     {<<"Radish Horse">>, [{drop, {<<"Horseradish">>,1}}, {experience, 2}]},
     {<<"Sand Worm">>, [{drop, {<<"Spices">>,10}}, {experience, 25}]},
     {<<"Mule">>, [{drop, {<<"Map">>, 2}}, {experience, 12}]},
     {<<"Man Tree">>, [{drop, {<<"branch">>,1}}, {experience, 2}]},
     {<<"Penguin Lord">>, [{drop, {<<"Penguin Egg">>,1}}, {experience, 3}]},
     {<<"Cursed Priest">>, [{drop, {<<"Grail">>, 3}}, {experience, 5}]},
     {<<"Bearded cow">>, [{drop, {<<"Hairy milk">>, 1}}, {experience, 6}]},
     {<<"Hellish crow">>, [{drop, {<<"Black feather">>, 1}}, {experience, 1}]},
     {<<"Wolverine">>, [{drop, {<<"Puddle of blood">>, 1}}, {experience, 2}]},
     {<<"Gangsta Bear">>, [{drop, {<<"Bear Grylls">>, 3}}, {experience, 4}]}].

