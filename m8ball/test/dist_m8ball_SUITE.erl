-module(dist_m8ball_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([can_contact/1]).

all() -> [{group, main}, {group, backup}].

groups() -> [{main,
             [],
             [can_contact]},
            {backup,
             [],
             [can_contact]}].

init_per_group(main, Config) ->
    application:start(crypto),
    application:start(m8ball),
    Config;
init_per_group(backup, Config) ->
    application:start(crypto),
    application:start(m8ball),
    Config.

end_per_group(main, _Config) ->
    %application:stop(m8ball),
    ok;
end_per_group(backup, _Config) ->
    %application:stop(m8ball),
    ok.

can_contact(_Config) ->
    <<_/binary>> = m8ball:ask(<<"Some Question">>).
