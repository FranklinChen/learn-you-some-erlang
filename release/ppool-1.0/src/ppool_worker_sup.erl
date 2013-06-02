-module(ppool_worker_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(MFA = {_,_,_}) ->
    supervisor:start_link(?MODULE, MFA).

init({M,F,A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
          [{ppool_worker,
            {M,F,A},
            temporary, 5000, worker, [M]}]}}.
    
