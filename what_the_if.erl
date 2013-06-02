-module(what_the_if).
-export([heh_fine/0, oh_god/1, help_me/1]).

%% should check if this actually works (hint: an error will be thrown)
heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
    if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 ->
        fails
    end.

oh_god(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does  %% this is Erlang's if's 'else!'
    end.

%% note, this one would be better as a pattern match in function heads!
%% I'm doing it this way for the sake of the example.
help_me(Animal) ->
    Talk = if Animal == cat  -> "meow";
              Animal == beef -> "mooo";
              Animal == dog  -> "bark";
              Animal == tree -> "bark";
              true -> "fgdadfgna"
           end,
    {Animal, "says " ++ Talk ++ "!"}.
