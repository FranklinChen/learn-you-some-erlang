-module(keyval_benchmark).
-compile(export_all).

%% Runs all benchmarks with Reps number of elements.
bench(Reps) ->
    io:format("Base Case:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(base_case(Reps)),
    io:format("~nNaive Orddict:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(naive_orddict(Reps)),
    io:format("~nSmart Orddict:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(smart_orddict(Reps)),
    io:format("~nNaive Dict:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(naive_dict(Reps)),
    io:format("~nSmart Dict:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(smart_dict(Reps)),
    io:format("~nNaive gb_trees:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(naive_gb_trees(Reps)),
    io:format("~nSmart gb_trees:~n"),
    io:format("Operation\tTotal (µs)\tAverage (µs)~n"),
    print(smart_gb_trees(Reps)).

%% formats the benchmark results cleanly.
print([]) -> ok;
print([{Op, Total, Avg} | Rest]) ->
    io:format("~8s\t~10B\t~.6f~n", [Op, Total, Avg]),
    print(Rest).

%% Example of a base benchmark function. This one actually does
%% nothing and can be used as a control for all the benchmark - it
%% will measure how much time it takes just to loop over data and
%% apply functions.
base_case(Reps) ->
    benchmark(Reps,                 % N repetitions
              [],                   % Empty data structure
              fun ?MODULE:null/3,   % Create
              fun ?MODULE:null/2,   % Read
              fun ?MODULE:null/3,   % Update
              fun ?MODULE:null/2).  % Delete

%% Ordered dictionaries. Assumes that the value is present on reads.
smart_orddict(Reps) ->
    benchmark(Reps,
              orddict:new(),
              fun orddict:store/3,
              fun orddict:fetch/2,
              fun orddict:store/3,
              fun orddict:erase/2).

%% Ordered dictionaries. Doesn't know whether a key is there or not on
%% reads.
naive_orddict(Reps) ->
    benchmark(Reps,
              orddict:new(),
              fun orddict:store/3,
              fun orddict:find/2,
              fun orddict:store/3,
              fun orddict:erase/2).

%% Dictionary benchmark. Assumes that the value is present on reads.
smart_dict(Reps) ->
    benchmark(Reps,
              dict:new(),
              fun dict:store/3,
              fun dict:fetch/2,
              fun dict:store/3,
              fun dict:erase/2).

%% Dictionary benchmark. Doesn't know if the value exisst at read time.
naive_dict(Reps) ->
    benchmark(Reps,
              dict:new(),
              fun dict:store/3,
              fun dict:find/2,
              fun dict:store/3,
              fun dict:erase/2).

%% gb_trees benchmark. Uses the most general functions -- i.e.: it never
%% assumes that the value is not in a tree when inserting and never assumes
%% it is there when updating or deleting.
naive_gb_trees(Reps) ->
    benchmark(Reps,
              gb_trees:empty(),
              fun gb_trees:enter/3,
              fun gb_trees:lookup/2,
              fun gb_trees:enter/3,
              fun gb_trees:delete_any/2).

%% gb_trees benchmark. Uses specific function: it assumes that the values
%% are not there when inserting and assumes it exists when updating or
%% deleting.
smart_gb_trees(Reps) ->
    benchmark(Reps,
              gb_trees:empty(),
              fun gb_trees:insert/3,
              fun gb_trees:get/2,
              fun gb_trees:update/3,
              fun gb_trees:delete/2).

%% Empty functions used for the 'base_case/1' benchmark. They must do
%% nothing interesting.
null(_, _) -> ok.
null(_, _, _) -> ok.

%% Runs all the functions of 4 formats: Create, Read, Update, Delete.
%% Create: it's a regular insertion, but it goes from an empty structure
%%         to a filled one. Requires an empty data structure,
%% Read: reads every key from a complete data structure.
%% Update: usually, this is the same as the insertion from 'create',
%%         except that it's run on full data structures. In some cases,
%%         like gb_trees, there also exist operations for updates when
%%         the keys are known that act differently from regular inserts.
%% Delete: removes a key from a tree. Because we want to test the
%%         efficiency of it all, we will always delete from a complete
%%         structure.
%% The function returns a list of all times averaged over the number
%% of repetitions (Reps) needed.
benchmark(Reps, Empty, CreateFun, ReadFun, UpdateFun, DeleteFun) ->
    Keys = make_keys(Reps),
    {TimeC, Struct} = timer:tc(?MODULE, create, [Keys, CreateFun, Empty]),
    {TimeR, _} = timer:tc(?MODULE, read, [Keys, Struct, ReadFun]),
    {TimeU, _} = timer:tc(?MODULE, update, [Keys, Struct, UpdateFun]),
    {TimeD, _} = timer:tc(?MODULE, delete, [Keys, Struct, DeleteFun]),
    [{create, TimeC, TimeC/Reps},
     {read, TimeR, TimeR/Reps},
     {update, TimeU, TimeU/Reps},
     {delete, TimeD, TimeD/Reps}].

%% Generate unique random numbers. No repetition allowed
make_keys(N) ->
    %% The trick is to generate all numbers as usual, but match them
    %% with a random value in a tuple of the form {Random, Number}.
    %% The idea is to then sort the list generated that way; done in
    %% this manner, we know all values will be unique and the randomness
    %% will be done by the sorting.
    Random = lists:sort([{random:uniform(N), X} || X <- lists:seq(1, N)]),
    %% it's a good idea to then filter out the index (the random index)
    %% to only return the real numbers we want. This is simple to do
    %% with a list comprehension where '_' removes the extraneous data.
    %% The keys are then fit into a tuple to make the test a bit more
    %% realistic for comparison.
    [{some, key, X} || {_, X} <- Random].

%% Loop function to apply the construction of a data structure.
%% The parameters passed are a list of all keys to use and then the
%% higher order function responsible of the creation of a data
%% structure. This is usually a function of the form
%% F(Key, Value, Structure).
%% In the first call, the structure has to be the empty data structure
%% that will progressively be filled.
%% The only value inserted for each key is 'some_data'; we only care
%% about the keys when dealing with key/value stuff.
create([], _, Acc) -> Acc;
create([Key|Rest], Fun, Acc) ->
    create(Rest, Fun, Fun(Key, some_data, Acc)).

%% Loop function to apply successive readings to a data structure.
%% The parameters passed are a list of all keys, the complete data
%% structure and then a higher order function responsible for
%% fetching the data. Such functions are usually of the form
%% F(Key, Structure).
read([], _, _) -> ok;
read([Key|Rest], Struct, Fun) ->
    Fun(Key, Struct),
    read(Rest, Struct, Fun).

%% Loop function to apply updates to a data structure.
%% Takes a list of keys, a full data structure and a higher order
%% function responsible for the updating, usually of the form
%% F(Key, NewValue, Structure).
%% All values for a given key are replaced by 'newval', again because
%% we don't care about the values, but merely the operations with
%% the keys.
update([], _, _) -> ok;
update([Key|Rest], Struct, Fun) ->
    Fun(Key, newval, Struct),
    update(Rest, Struct, Fun).

%% Loop function to apply deletions to a data structure.
%% Each deletion is made on a full data structure.
%% Takes a list of keys, a data structure and a higher order function
%% to do the deletion. Usually of the form
%% F(Key, Structure).
delete([], _, _) -> ok;
delete([Key|Rest], Struct, Fun) ->
    Fun(Key, Struct),
    delete(Rest, Struct, Fun).
