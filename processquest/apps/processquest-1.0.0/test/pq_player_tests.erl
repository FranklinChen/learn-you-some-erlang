-module(pq_player_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {name, stats, exp=0, lvlexp=1000, lvl=1, % copied from pq_player.erl
                 equip=[], money=0, loot=[], bought=[], time=0}).

-define(setup(Name, T), {setup, fun() -> start(Name) end, fun stop/1, fun T/1}).
-define(setup(T), ?setup(make_ref(), T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
new_player_test_() ->
    [{"A player holds its own name in its state",
      ?setup(initial_name)},
     {"A new player has stats randomly rolled",
      ?setup(initial_roll)},
     {"A new player becomes a registered process",
      ?setup(initial_register)},
     {"A player has a counter for experience and a counter for "
      "the next level (which is higher than the current exp)",
      ?setup(initial_lvl)},
     {"A player has a basic equipment (empty) when first going",
      ?setup(initial_equipment)},
     {"A new player has no money",
      ?setup(initial_money)},
     {"A new player has no loot",
      ?setup(initial_loot)},
     {"The state of a player can be overriden using the info "
      "arguments to the init function",
      ?setup(override_init)}].

market_test_() ->
    [{"A player with N items will sell all of them to the market "
      "and end with equivalent money, before switching to the "
      "buying state",
      ?setup(sell_all)},
     {"A player with nearly infinite money will buy items available "
      "for his money, higher than his level",
      ?setup(buy_items)},
     {"A player with no money or no items available for the price "
      "range leaves for the killing fields.",
      ?setup(buy_none)},
     {"Receiving the kill message just forwards to the killing state",
      ?setup(to_killing)}].

killing_fields_test_() ->
    [{"Kill enemies until the loot limit is hit. Loot is 2x Strength",
      ?setup(loot_limit)},
     {"Killing enemies raises XP until someone levels up",
      ?setup(kill_xp)},
     {"Leveling up boosts stats. The sum is higher by at least as "
      "many points as there are fields, but not more than 6 times. "
      "Moreover, the rolling is random.",
      ?setup(lvl_stats)},
     {"receiving the market message just forwards to the market state",
      ?setup(to_market)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(Name) ->
    application:start(crypto),
    application:start(regis),
    Pid = spawn(fun() -> timer:sleep(infinity) end),
    regis:register({events, Name}, Pid),
    Name.

stop(Name) ->
    exit(regis:whereis({events, Name}), kill),
    application:stop(regis),
    application:stop(crypto).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Initial State
initial_name(Name) ->
    {ok, market, S} = pq_player:init({Name, []}),
    M = read_event(),
    [?_assertEqual(Name, S#state.name),
     ?_assertEqual(M, kill)].

initial_roll(Name) ->
    {ok, _, S} = pq_player:init({Name, []}),
    _ = read_event(),
    ?_assertMatch([{charisma,_}, {constitution, _}, {dexterity, _},
                   {intelligence, _}, {strength, _}, {wisdom, _}],
                  lists:sort(S#state.stats)).

initial_register(Ref) ->
    {ok, _, _} = pq_player:init({Ref, []}),
    _ = read_event(),
    Pid = regis:whereis(Ref),
    Ret = pq_player:init({Ref, []}),
    _ = read_event(),
    [?_assert(undefined =/= Pid),
     ?_assert(is_pid(Pid)),
     ?_assertEqual({stop, name_taken}, Ret)].

initial_lvl(Name) ->
    {ok, _, S} = pq_player:init({Name, []}),
    _ = read_event(),
    [?_assert(is_integer(S#state.lvlexp)),
     ?_assert(S#state.lvlexp > 0),
     ?_assert(S#state.exp =:= 0)]. % start at 0 exp

initial_equipment(Name) ->
    {ok, _, S} = pq_player:init({Name, []}),
    _ = read_event(),
    [?_assert(is_list(S#state.equip)),
     ?_assertEqual(undefined, proplists:get_value(armor, S#state.equip)),
     ?_assertEqual(undefined, proplists:get_value(helmet, S#state.equip)),
     ?_assertEqual(undefined, proplists:get_value(weapon, S#state.equip)),
     ?_assertEqual(undefined, proplists:get_value(shield, S#state.equip))].

initial_money(Name) ->
    {ok, _, S} = pq_player:init({Name, []}),
    _ = read_event(),
    ?_assertEqual(0, S#state.money).

initial_loot(Name) ->
    {ok, _, S} = pq_player:init({Name, []}),
    _ = read_event(),
    ?_assertEqual([], S#state.loot).

override_init(Name) ->
    {ok, _, Partial} = pq_player:init({Name, [
        {stats, [{charisma,1}, {constitution,1}, {dexterity,1},
                 {intelligence,1}, {strength,1}, {wisdom,1}]},
        {lvlexp,1}]}),
    regis:unregister(Name),
    _ = read_event(),
    {ok, _, Complete} = pq_player:init({Name, [
        {stats, [{charisma,1}, {constitution,1}, {dexterity,1},
                 {intelligence,1}, {strength,1}, {wisdom,1}]},
        {exp, 1}, {lvlexp,1}, {lvl,9},
        {equip, [{weapon,{<<"plastic knife">>, -1, 1, 2}}]},
        {money,1}, {loot, [{<<"Bacon">>, 1}]}, {bought, [helmet]}
    ]}),
    _ = read_event(),
    [?_assertMatch(#state{stats=[{_,1},{_,1},{_,1},{_,1},{_,1},{_,1}],
                          lvlexp=1, lvl=1, exp=0},
                   Partial),
     ?_assertMatch(#state{stats=[{_,1},{_,1},{_,1},{_,1},{_,1},{_,1}],
                          exp=1, lvlexp=1, lvl=9, equip=[{weapon,_}],
                          money=1, loot=[{_,_}], bought=[_]},
                   Complete)].

%% Market
sell_all(Name) ->
    undefined = read_event(),
    Loot = [proplists:get_value(drop, element(2, pq_enemy:fetch()))
             || _ <- lists:seq(1,5)],
    {[Sum1, Sum2, Sum3, Sum4, Sum5], _} = lists:mapfoldl(
        fun(X, Sum) -> {X+Sum, X+Sum} end,
        0,
        [Val || {_, Val} <- Loot]
    ),
    %undefined = read_event(),
    S0 = #state{name=Name, loot=Loot, money=0, lvl=1},
    {next_state, market, S1} = pq_player:market(sell, S0),
    M1 = read_event(),
    {next_state, market, S2} = pq_player:market(sell, S1),
    M2 = read_event(),
    {next_state, market, S3} = pq_player:market(sell, S2),
    M3 = read_event(),
    {next_state, market, S4} = pq_player:market(sell, S3),
    M4 = read_event(),
    {next_state, market, S5} = pq_player:market(sell, S4),
    M5 = read_event(),
    {next_state, market, S6} = pq_player:market(sell, S5),
    M6 = read_event(),
    [?_assertMatch(#state{money=Sum1, loot=[_,_,_,_]}, S1),
     ?_assertMatch(#state{money=Sum2, loot=[_,_,_]}, S2),
     ?_assertMatch(#state{money=Sum3, loot=[_,_]}, S3),
     ?_assertMatch(#state{money=Sum4, loot=[_]}, S4),
     ?_assertMatch(#state{money=Sum5, loot=[]}, S5),
     ?_assertMatch(#state{money=Sum5, loot=[]}, S6),
     ?_assertEqual([sell, sell, sell, sell, sell, buy],
                   [M1,M2,M3,M4,M5,M6])].

buy_items(Name) ->
    %% 4 different pieces of equipment to buy
    S0 = #state{name=Name, equip=[], money=999999999999},
    {next_state, market, S1} = pq_player:market(buy, S0),
    M1 = read_event(),
    {next_state, market, S2} = pq_player:market(buy, S1),
    M2 = read_event(),
    {next_state, market, S3} = pq_player:market(buy, S2),
    M3 = read_event(),
    {next_state, market, S4} = pq_player:market(buy, S3),
    M4 = read_event(),
    %% All slots bought. Implicit requirement: not buying for the
    %% same slot twice.
    {next_state, market, S5} = pq_player:market(buy, S4),
    M5 = read_event(),
    [?_assertEqual([S5#state.money, S4#state.money, S3#state.money,
                    S2#state.money, S1#state.money, S0#state.money],
                   lists:sort([S5#state.money, S4#state.money, S3#state.money,
                          S2#state.money, S1#state.money, S0#state.money])),
     ?_assertEqual([1,2,3,4,4],
                   [length(L) || L <- [S1#state.equip, S2#state.equip,
                                       S3#state.equip, S4#state.equip,
                                       S5#state.equip]]),
     ?_assertEqual([buy, buy, buy, buy, kill],
                   [M1, M2, M3, M4, M5])]. 

buy_none(Name) ->
    S0 = #state{name=Name, equip=[], money=0},
    %% one try per part of the equipment
    {next_state, market, S1} = pq_player:market(buy, S0),
    _ = read_event(),
    {next_state, market, S2} = pq_player:market(buy, S1),
    _ = read_event(),
    {next_state, market, S3} = pq_player:market(buy, S2),
    _ = read_event(),
    {next_state, market, S4} = pq_player:market(buy, S3),
    M = read_event(),
    [?_assertEqual(S0, S4),
     ?_assertEqual(kill, M)].

to_killing(Name) ->
    S = #state{name=Name},
    Res = pq_player:market(kill, S),
    M = read_event(),
    [?_assertMatch({next_state, killing, S}, Res),
     ?_assertEqual(kill, M)].

%% Killing fields tests
loot_limit(Name) ->
    S0 = #state{name=Name, stats=[{strength, 2}], loot=[]},
    {next_state, killing, S1 = #state{loot=L1}} = pq_player:killing(kill, S0),
    M1 = read_event(),
    {next_state, killing, S2 = #state{loot=L2}} = pq_player:killing(kill, S1),
    M2 = read_event(),
    {next_state, killing, S3 = #state{loot=L3}} = pq_player:killing(kill, S2),
    M3 = read_event(),
    {next_state, killing, #state{loot=L4}} = pq_player:killing(kill, S3),
    M4 = read_event(),
    %% Group identical drops with a counter?
    [?_assertEqual([1,2,3,4], [length(L) || L <- [L1, L2, L3, L4]]),
     ?_assertEqual([kill, kill, kill, market], [M1, M2, M3, M4])].

kill_xp(Name) ->
    S0 = #state{name=Name, stats=[{strength, 999}|pq_stats:initial_roll()],
                lvl=1, exp=0, lvlexp=5},
    %% between 1 and 5 kills required to lvl up.
    {next_state, NS1, S1} = pq_player:killing(kill, S0),
    M1 = read_event(),
    {next_state, NS2, S2} = pq_player:NS1(M1, S1),
    M2 = read_event(),
    {next_state, NS3, S3} = pq_player:NS2(M2, S2),
    M3 = read_event(),
    {next_state, NS4, S4} = pq_player:NS3(M3, S3),
    M4 = read_event(),
    {next_state, NS5, S5} = pq_player:NS4(M4, S4),
    M5 = read_event(),
    {next_state, NS6, S6} = pq_player:NS5(M5, S5),
    M6 = read_event(),
    [?_assert(lists:any(fun(#state{lvl=L}) -> L > 1 end, [S1,S2,S3,S4,S5,S6])),
     ?_assert(lists:any(fun(#state{lvlexp=L}) -> L >= 10 end, [S1,S2,S3,S4,S5,S6])),
     ?_assert(lists:any(fun(#state{exp=E}) -> E >= 5 end, [S1,S2,S3,S4,S5,S6])),
     ?_assert(lists:any(fun(FSMState) -> FSMState =:= killing end,
                        [NS1, NS2, NS3, NS4, NS5, NS6])),
     ?_assert(lists:any(fun(Msg) -> Msg =:= kill end,
                        [M1, M2, M3, M4, M5, M6])),
     ?_assert(lists:any(fun(Msg) -> Msg =:= lvl_up end,
                        [M1, M2, M3, M4, M5, M6]))].

lvl_stats(Name) ->
    {ok, _, S0} = pq_player:init({Name, []}),
    _ = read_event(),
    TotalStats = length(S0#state.stats),
    {next_state, killing, S1} = pq_player:killing(lvl_up, S0),
    _ = read_event(),
    {next_state, killing, S2} = pq_player:killing(lvl_up, S0),
    _ = read_event(),
    SumInit = lists:sum([Pts || {_,Pts} <- S0#state.stats]),
    SumS1 = lists:sum([Pts || {_,Pts} <- S1#state.stats]),
    SumS2 = lists:sum([Pts || {_,Pts} <- S2#state.stats]),
    [?_assert(SumS1 >= TotalStats+SumInit),
     ?_assert(SumS2 >= TotalStats+SumInit),
     ?_assert(SumS1 =< TotalStats*6 + SumInit),
     ?_assert(SumS2 =< TotalStats*6 + SumInit),
     ?_assert(S1#state.stats =/= S2#state.stats)].

to_market(Name) ->
    S = #state{name=Name},
    Res = pq_player:killing(market, S),
    M = read_event(),
    [?_assertMatch({next_state, market, S}, Res),
     ?_assertEqual(sell, M)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
read_event() ->
    receive
        {'$gen_event', Msg} -> Msg
    after 0 ->
        undefined
    end.
