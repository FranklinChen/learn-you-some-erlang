%%% pq_supersup is the ProcessQuest top-level supervisor.
%%% It sits over many pq_sup instances, allowing to have
%%% a truckload of different players running at once.
-module(pq_supersup).
-behaviour(supervisor).
-export([start_link/0, start_player/2, stop_player/1]).
-export([init/1]).

%% We register it so that it's guaranteed to be unique
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Using a SOFO strategy because we get to have many
%% supervisees of the same type.
init([]) ->
    {ok,
     {{simple_one_for_one, 1, 60000},
      [{sup,
        {pq_sup, start_link, []},
        permanent, infinity, supervisor, [pq_sup]}]}}.

%% Starts an individual player
start_player(Name, Info) ->
    supervisor:start_child(?MODULE, [Name, Info]).

%% Stops a player.
stop_player(Name) ->
    supervisor:terminate_child(?MODULE, regis:whereis(Name)).
