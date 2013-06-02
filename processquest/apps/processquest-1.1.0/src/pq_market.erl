%%% Can be used to obtain weapons and pieces of equipment of various types
%%% to be equipped by the hero. The standard format is:
%%%  {Name, LevelModifier, Level, Price}.
-module(pq_market).
-export([helmet/2, weapon/2, shield/2, armor/2]).

weapon(CombinedLvl, Money) ->
    L = [
     {<<"plastic knife">>, -1, 1, 2},
     {<<"plastic knife">>, 0, 1, 3},
     {<<"plastic knife">>, 1, 1, 5},
     {<<"metal spoon">>, -1, 4, 3},
     {<<"metal spoon">>, 0, 4, 4},
     {<<"butter knife">>, -1, 6, 5},
     {<<"butter knife">>, 0, 6, 7},
     {<<"butter knife">>, 1, 6, 9},
     {<<"machete">>, -1, 9, 15},
     {<<"machete">>, 0, 9, 20},
     {<<"machete">>, 1, 9, 25},
     {<<"broad sword">>, -1, 12, 23},
     {<<"broad sword">>, 0, 12, 30},
     {<<"broad sword">>, 1, 12, 38},
     {<<"lance">>, -1, 15, 32},
     {<<"lance">>, 0, 15, 44},
     {<<"lance">>, 1, 15, 57},
     {<<"pistol">>, -1, 25, 95},
     {<<"pistol">>, 0, 25, 105},
     {<<"pistol">>, 1, 25, 155},
     {<<"submachine gun">>, -1, 40, 200},
     {<<"submachine gun">>, 0, 40, 245},
     {<<"submachine gun">>, 1, 40, 365}
    ],
    first_match(fun(W = {_, Modifier, Lvl, Price}) ->
            if Modifier+Lvl > CombinedLvl, Price =< Money -> W;
                true -> continue
            end
        end, L).

helmet(CombinedLvl, Money) -> pick_material(CombinedLvl, Money).
shield(CombinedLvl, Money) -> pick_material(CombinedLvl, Money).
armor(CombinedLvl, Money) -> pick_material(CombinedLvl, Money).

pick_material(CombinedLvl, Money) ->
    L = materials(),
    first_match(fun(W = {_, Modifier, Lvl, Price}) ->
            if Modifier+Lvl > CombinedLvl, Price =< Money -> W;
                true -> continue
            end
        end, L).


first_match(_, []) -> undefined;
first_match(F, [H|T]) ->
    case F(H) of
        continue -> first_match(F,T);
        Val -> Val
    end.

materials() ->
    [{<<"wool">>, 0, 1, 25},
     {<<"pleather">>, 0, 2, 45},
     {<<"pleather">>, 1, 2, 50},
     {<<"pleather">>, 2, 2, 65},
     {<<"leather">>, -2, 7, 30},
     {<<"leather">>, -1, 7, 35},
     {<<"leather">>, 0, 7, 45},
     {<<"leather">>, 2, 7, 65},
     {<<"chain mail">>, -2, 12, 70},
     {<<"chain mail">>, 0, 12, 85},
     {<<"chain mail">>, 1, 12, 95},
     {<<"chain mail">>, 2, 12, 105},
     {<<"plate mail">>, -2, 17, 90},
     {<<"plate mail">>, -1, 17, 95},
     {<<"plate mail">>, 0, 17, 105},
     {<<"plate mail">>, 1, 17, 115},
     {<<"plate mail">>, 2, 17, 135}].

