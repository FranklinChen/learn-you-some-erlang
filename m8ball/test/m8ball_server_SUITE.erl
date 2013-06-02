-module(m8ball_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([random_answer/1, binary_answer/1, determined_answers/1,
         reload_answers/1]).

all() -> [random_answer, binary_answer, determined_answers,
          reload_answers].

init_per_testcase(_Test, Config) ->
    Answers = [<<"Outlook not so good">>,
               <<"I don't think so">>,
               <<"Yes, definitely">>,
               <<"STOP SHAKING ME">>],
    application:set_env(m8ball, answers, list_to_tuple(Answers)),
    m8ball_server:start_link(),
    [{answers, Answers} | Config].

end_per_testcase(_Test, _Config) ->
    m8ball_server:stop().

%% The answer should come from the config
random_answer(_Config) ->
    Answers1 = [m8ball_server:ask("Dummy question") || _ <- lists:seq(1,15)],
    Answers2 = [m8ball_server:ask("Dummy question") || _ <- lists:seq(1,15)],
    true = 1 < length(sets:to_list(sets:from_list(Answers1))),
    true = Answers1 =/= Answers2.

binary_answer(_Config) ->
    Answers = [m8ball_server:ask("Dummy question") || _ <- lists:seq(1,15)],
    L = length(Answers),
    L = length(lists:filter(fun erlang:is_binary/1, Answers)).

determined_answers(Config) ->
    Answers = ?config(answers, Config),
    Res = [m8ball_server:ask("Dummy question") || _ <- lists:seq(1,15)],
    true = lists:all(fun(X) -> lists:member(X, Answers) end, Res).

reload_answers(_Config) ->
    Ref = make_ref(),
    application:set_env(m8ball, answers, {Ref}),
    [Ref,Ref,Ref] = [m8ball_server:ask("Question") || _ <- lists:seq(1,3)].
    

%% NOTE: do distributed testing in m8ball_SUITE or something
