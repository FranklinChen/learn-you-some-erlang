-module(meeting_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([carla/1, mark/1, dog/1, all_same_owner/1]).

%% Regroup the tests as groups that will let you try to see if the 'meeting.erl'
%% modules contains concurrency errors or not.

all() -> [{group, session}].

groups() -> [{session,
              [],
              [{group, clients}, all_same_owner]},
             {clients,
              [parallel, {repeat, 10}],
              [carla, mark, dog]}].

init_per_group(session, Config) ->
    meeting:start(),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(session, _Config) ->
    meeting:stop();
end_per_group(_, _Config) ->
    ok.

carla(_Config) ->
    meeting:book_room(women),
    timer:sleep(10),
    meeting:rent_projector(women),
    timer:sleep(10),
    meeting:use_chairs(women).

mark(_Config) ->
    meeting:rent_projector(men),
    timer:sleep(10),
    meeting:use_chairs(men),
    timer:sleep(10),
    meeting:book_room(men).

dog(_Config) ->
    meeting:rent_projector(animals),
    timer:sleep(10),
    meeting:use_chairs(animals),
    timer:sleep(10),
    meeting:book_room(animals).

all_same_owner(_Config) ->
    [{_,Owner}, {_, Owner}, {_, Owner}] = meeting:get_all_bookings().
