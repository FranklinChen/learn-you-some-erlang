%%% Translates the process quest events to iolists
%%% that can be sent over a socket.
%%%
%%% IO lists are lists of bytes (0..255, ASCII),
%%% binaries and other iolists. They allow to append,
%%% prepend and insert data in strings without re-writing
%%% the fragments that compose them. Erlang's drivers and
%%% IO modules accept them without an issue and are a quick,
%%% somewhat elegant solution to immutable data structures
%%% requiring many changes.
-module(sockserv_trans).
-export([to_str/1]).

%% The player killed something
to_str({_User, killed, Time, {EnemyName, Props}}) ->
    {Drop, _} = proplists:get_value(drop, Props),
    [["Executing a ",EnemyName, "..."],
     {wait, Time}, % take a pause between the output values
     ["Obtained ", Drop, "."]];
%% Changing locations
to_str({_Name, heading, _Time, Loc}) ->
    [["Heading to ",
      case Loc of
        market -> "the marketplace to sell loot...";
        killing -> "the killing fields..."
      end]];
%% Leveling up
to_str({_Name, lvl_up, _, NewStats, NewLvl, _NewExp}) ->
    [["Leveled up to level ", integer_to_list(NewLvl),
      " Here are your new stats:", $\n,
      io_lib:format(
         "  Charisma: ~B~n"
         "  Constitution: ~B~n"
         "  Dexterity: ~B~n"
         "  Intelligence: ~B~n"
         "  Strength: ~B~n"
         "  Wisdom: ~B~n~n",
         [Points || {_, Points} <- lists:sort(NewStats)])]];
%% Bought an item
to_str({_Name, buy, Time, Slot, {Item, _, _, _}}) ->
    SlotTxt = case Slot of
        armor -> " armor";
        weapon -> "";
        helmet -> " helmet";
        shield -> " shield"
    end,
    [["Negotiating purchase of better equipment..."],
     {wait, Time},
     ["Bought a ", Item, SlotTxt]];
%% Sold an item
to_str({_Name, sell, Time, {Item, Val}}) ->
    [["Selling ", Item],
     {wait, Time},
     ["Got ", integer_to_list(Val), " bucks."]];
%% Completed a quest
to_str({_Name, quest, 0, Completed, New}) ->
    [["Completed quest: ", Completed, "..."],
     ["Obtained new quest: ", New, "."]].
