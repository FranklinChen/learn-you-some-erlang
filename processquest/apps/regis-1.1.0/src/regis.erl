%%% Application wrapper module for regis,
%%% a process registration application.
%%%
%%% This was added because the standard process registry has a precise
%%% meaning of representing VM-global, non-dynamic processes.
%%% However, for this, we needed dynamic names and so we had to write
%%% one ourselves. Of course we could have used 'global' (but we
%%% didn't see distributed Erlang yet) or 'gproc' (I don't want to
%%% depend on external libs for this guide), so checkthem out
%%% if you're writing your own app.
-module(regis).
-behaviour(application).
-export([start/2, stop/1]).
-export([register/2, unregister/1, whereis/1, get_names/0]).


start(normal, []) ->
    regis_sup:start_link().

stop(_) ->
    ok.

register(Name, Pid) -> regis_server:register(Name, Pid).

unregister(Name) -> regis_server:unregister(Name).

whereis(Name) -> regis_server:whereis(Name).

get_names() -> regis_server:get_names().
