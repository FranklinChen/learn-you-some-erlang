-module(oop_tests).
-include_lib("eunit/include/eunit.hrl").

shell_test_() ->
    Bird = oop:animal("Bird"),
    Dog = oop:dog("Raptor-Dog"),
    Cat = oop:cat("Sgt. McMittens"),
    [?_assertEqual("living thing", Bird(type)),
     ?_assertEqual("Bird eats worm", Bird({eat, "worm"})),
     ?_assertEqual("Raptor-Dog says: Woof!", Dog(talk)),
     ?_assertEqual("Raptor-Dog", Dog(name)),
     ?_assertEqual("cat", Cat(type)),
     ?_assertEqual("Raptor-Dog chases a cat named Sgt. McMittens around",
                   Dog({chase, Cat})),
     ?_assertEqual("I'm sorry Dave, I can't do that.", Cat({play, "yarn"}))].
     
