-module(tester).
-export([dir/0, dir/2]).
-define(EXT, ".erl"). % file extension to look for
-define(MODS, "./").
-define(TESTS, "./tests/").

%% scans both a module directory and a test directory, compiles the
%% modules inside and then call for tests to be ran.
%%
%% usage:
%%     tester:dir("./","./tests/").
dir() -> dir(?MODS, ?TESTS).
dir(ModulePath, TestPath) ->
    ModuleList = module_list(ModulePath),
    TestList = module_list(TestPath),
    [compile(ModulePath++X) || X <- ModuleList],
    [compile(TestPath++X) || X <- TestList],
    test_all(TestList),
    warnings(),
    cleanup(ModuleList),
    cleanup(TestList),
    ok.

%% assumes pre-compiled modules
test_all(FileList) ->
    Split = [lists:nth(1, string:tokens(File, ".")) || File <- FileList],
    [eunit:test(list_to_existing_atom(F), [verbose]) || F <- Split].
   % [(list_to_existing_atom(F)):test() || F <- Split].

cleanup(Files) ->
    [file:delete(lists:nth(1, string:tokens(F, "."))++".beam") || F <- Files].

%% get module .erl file names from a directory
module_list(Path) ->
    SameExt = fun(File) -> get_ext(File) =:= ?EXT end,
    {ok, Files} = file:list_dir(Path),
    lists:filter(SameExt, Files).

%% find the extension of a file (length is taken from the ?EXT macro).
get_ext(Str) ->
    lists:reverse(string:sub_string(lists:reverse(Str), 1, length(?EXT))).

compile(FileName) ->
    compile:file(FileName, [report, verbose, export_all]).

warnings() ->
    Warns = [{Mod, get_warnings(Mod)} || {Mod,_Path} <- code:all_loaded(),
                                         has_warnings(Mod)],
    io:format("These need to be tested better: ~n\t~p~n", [Warns]).

has_warnings(Mod) ->
    is_list(get_warnings(Mod)).

get_warnings(Mod) ->
    proplists:get_value(test_warnings, Mod:module_info(attributes)).
