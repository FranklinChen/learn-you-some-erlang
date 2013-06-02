%%% The top-level supervisor of the registration
%%% server.
-module(regis_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 3600},
          [{server,
           {regis_server, start_link, []},
           permanent,
           500,
           worker,
           [regis_server]}]}}.
