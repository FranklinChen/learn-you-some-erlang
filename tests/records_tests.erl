-module(records_tests).
-include_lib("eunit/include/eunit.hrl").

first_robot_test_() ->
    ?_assertEqual(records:first_robot(),
        {robot, 
         "Mechatron",
         handmade,
         undefined,
         ["Moved by a small man inside"]}).

car_factory_test_() ->
    ?_assertEqual(records:car_factory("Jokeswagen"),
                  {robot,
                   "Jokeswagen",
                   industrial,
                   "building cars",
                   []}).

repairman_test_() ->
    ?_assertEqual(records:repairman({robot,
                                     "Ulbert",
                                     industrial,
                                     ["trying to have feelings"],
                                     []}),
                  {repaired, {robot,
                              "Ulbert",
                              industrial,
                              ["trying to have feelings"],
                              ["Repaired by repairman"]}}).

admin_panel_test_() ->
    [?_assertEqual(records:admin_panel({user, 1, "ferd", admin, 96}),
                   "ferd is allowed!"),
     ?_assertEqual(records:admin_panel({user, 2, "you", users, 66}),
                   "you is not allowed")].

adult_section_test_() ->
    [?_assertEqual(records:adult_section({user, 21, "Bill", users, 72}),
                   allowed),
     ?_assertEqual(records:adult_section({user, 22, "Noah", users, 13}),
                   forbidden)].

included_test_() ->
    ?_assertEqual(records:included(),
                  {included, "Some value", "yeah!", undefined}).
