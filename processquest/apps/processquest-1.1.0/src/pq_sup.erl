%%% Supervisor for each player. Goes over a pair of a
%%% gen_fsm (pq_player) and gen_event (pq_events).
-module(pq_sup).
-behaviour(supervisor).
-export([start_link/2]).
-export([init/1]).


start_link(Name, Info) ->
    supervisor:start_link(?MODULE, {Name,Info}).

%% The name is passed to the events so that
%% it can register itself as {events, Name} into the
%% 'regis' regsitry app.
%% Same for pq_player, which also gets the info.
%%
%% It is important that pq_events is started before
%% pq_player, otherwise we might create race conditions
%% when starting a player and then quickly generating events to
%% an event manager that doesn't exist.
init({Name, Info}) ->
    {ok,
     {{one_for_all, 2, 3600},
      [{events,
        {pq_events, start_link, [Name]},
        permanent, 5000, worker, [dynamic]},
       {player,
        {pq_player, start_link, [Name, Info]},
        permanent, 2000, worker, [pq_player]}]}}.


