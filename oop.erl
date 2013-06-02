-module(oop).
-export([animal/1, dog/1, cat/1]).

%% all the method calls need to be in tuples when they have more than
%% one argument so we can use functions of arity 1 for every call we make.

animal(Name) ->
    fun(type) -> "living thing";
       (name) -> Name;
       (move) -> Name++" moves around...";
       ({eat, Item}) -> Name++" eats "++Item;
       (_) -> "I'm sorry Dave, I can't do that."
    end.

dog(Name) ->
    Parent = animal(Name),
    fun(talk) -> Name++" says: Woof!";
       ({chase, Animal}) when is_function(Animal) ->
            Name++" chases a "++Animal(type)++" named "++Animal(name)++" around";
       (X) -> Parent(X)
    end.

cat(Name) ->
    Parent = animal(Name),
    fun(type) -> "cat";
       (talk) -> Name++" says: Meow!";
       (X) -> Parent(X)
    end.
