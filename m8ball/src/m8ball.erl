-module(m8ball).
-behaviour(application).
-export([start/2, stop/1]).
-export([ask/1]).

%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

%% start({failover, Node}, Args) is only called
%% when a start_phase key is defined.
start(normal, []) ->
    m8ball_sup:start_link();
start({takeover, _OtherNode}, []) ->
    m8ball_sup:start_link().

stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
ask(Question) ->
    m8ball_server:ask(Question).
