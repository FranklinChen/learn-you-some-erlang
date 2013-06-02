-module(mafiapp_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         all/0]).
-export([add_service/1, friend_by_name/1, friend_by_expertise/1,
         friend_with_services/1, accounts/1, enemies/1]).

all() -> [add_service, friend_by_name, friend_by_expertise,
          friend_with_services, accounts, enemies].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:load(mnesia),
    application:set_env(mnesia, dir, Priv),
    application:load(mafiapp),
    mafiapp:install([node()]),
    application:start(mnesia),
    application:start(mafiapp),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(add_service, Config) ->
    Config;
init_per_testcase(accounts, Config) ->
    ok = mafiapp:add_friend("Consigliere", [], [you], consigliere),
    Config;
init_per_testcase(_, Config) ->
    ok = mafiapp:add_friend("Don Corleone", [], [boss], boss),
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% services can go both way: from a friend to the boss, or
%% from the boss to a friend! A boss friend is required!
add_service(_Config) ->
    {error, unknown_friend} = mafiapp:add_service("from name",
                                                  "to name",
                                                  {1946,5,23},
                                                  "a fake service"),
    ok = mafiapp:add_friend("Don Corleone", [], [boss], boss),
    ok = mafiapp:add_friend("Alan Parsons",
                            [{twitter,"@ArtScienceSound"}],
                            [{born, {1948,12,20}},
                             musician, 'audio engineer',
                             producer, "has projects"],
                            mixing),
    ok = mafiapp:add_service("Alan Parsons", "Don Corleone",
                             {1973,3,1},
                             "Helped release a Pink Floyd album").

friend_by_name(_Config) ->
    ok = mafiapp:add_friend("Pete Cityshend",
                            [{phone, "418-542-3000"},
                             {email, "quadrophonia@example.org"},
                             {other, "yell real loud"}],
                            [{born, {1945,5,19}},
                             musician, popular],
                            music),
    {"Pete Cityshend",
     _Contact, _Info, music,
     _Services} = mafiapp:friend_by_name("Pete Cityshend"),
    undefined = mafiapp:friend_by_name(make_ref()).

friend_by_expertise(_Config) ->
    ok = mafiapp:add_friend("A Red Panda",
                            [{location, "in a zoo"}],
                            [animal,cute],
                            climbing),
    [{"A Red Panda",
      _Contact, _Info, climbing,
     _Services}] = mafiapp:friend_by_expertise(climbing),
    [] = mafiapp:friend_by_expertise(make_ref()).

friend_with_services(_Config) ->
    ok = mafiapp:add_friend("Someone", [{other, "at the fruit stand"}],
                            [weird, mysterious], shadiness),
    ok = mafiapp:add_service("Don Corleone", "Someone",
                             {1949,2,14}, "Increased business"),
    ok = mafiapp:add_service("Someone", "Don Corleone",
                             {1949,12,25}, "Gave a Christmas gift"),
    %% We don't care about the order. The test was made to fit
    %% whatever the functions returned.
    {"Someone",
     _Contact, _Info, shadiness,
     [{to, "Don Corleone", {1949,12,25}, "Gave a Christmas gift"},
      {from, "Don Corleone", {1949,2,14}, "Increased business"}]} =
    mafiapp:friend_by_name("Someone").

%% It should be possible to find all people who owe us things.
accounts(_Config) ->
    ok = mafiapp:add_friend("Gill Bates", [{email, "ceo@macrohard.com"}],
                            [clever,rich], computers),
    ok = mafiapp:add_service("Consigliere", "Gill Bates",
                             {1985,11,20}, "Bought 15 copies of software"),
    ok = mafiapp:add_service("Gill Bates", "Consigliere",
                             {1986,8,17}, "Made computer faster"),
    ok = mafiapp:add_friend("Pierre Gauthier", [{other, "city arena"}],
                            [{job, "sports team GM"}], sports),
    ok = mafiapp:add_service("Pierre Gauthier", "Consigliere", {2009,6,30},
                             "Took on a huge, bad contract"),
    ok = mafiapp:add_friend("Wayne Gretzky", [{other, "Canada"}],
                            [{born, {1961,1,26}}, "hockey legend"],
                            hockey),
    ok = mafiapp:add_service("Consigliere", "Wayne Gretzky", {1964,1,26},
                             "Gave first pair of ice skates"),
    %% Wayne Gretzky owes us something so the debt is negative
    %% Gill Bates are equal
    %% Gauthier is owed a service.
    [{-1,"Wayne Gretzky"},
     {0,"Gill Bates"},
     {1,"Pierre Gauthier"}] = mafiapp:debts("Consigliere"),
    [{1, "Consigliere"}] = mafiapp:debts("Wayne Gretzky").

enemies(_Config) ->
    undefined = mafiapp:find_enemy("Edward"),
    ok = mafiapp:add_enemy("Edward", [{bio, "Vampire"},
                                  {comment, "He sucks (blood)"}]),
    {"Edward", [{bio, "Vampire"},
                {comment, "He sucks (blood)"}]} =
       mafiapp:find_enemy("Edward"),
    ok = mafiapp:enemy_killed("Edward"),
    undefined = mafiapp:find_enemy("Edward").

