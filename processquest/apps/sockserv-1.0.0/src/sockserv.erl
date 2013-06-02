%%% Starting the sockserv application.
%%% The sockserv application is a lightweight
%%% Raw socket server that can be used with telnet
%%% to follow updates on the process quest game.
%%% The port is defined in the app's env
-module(sockserv).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    sockserv_sup:start_link().

stop(_) -> ok.
