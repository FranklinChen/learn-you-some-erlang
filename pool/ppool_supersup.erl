-module(ppool_supersup).
-behaviour(supervisor).
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

%% technically, a supervisor can not be killed in an easy way.
%% Let's do it brutally!
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                 {ppool_sup, start_link, [Name, Limit, MFA]},
                  permanent, 10500, supervisor, [ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).

init([]) ->
    MaxRestart = 6,
    MaxTime = 3000,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.
