%%% Converts events from a player's event manager into a
%%% cast sent to the sockserv socket gen_server.
-module(sockserv_pq_events).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

init(Parent) -> {ok, Parent}.

handle_event(E, Pid) ->
    gen_server:cast(Pid, E),
    {ok, Pid}.

handle_call(Req, Pid) ->
    Pid ! Req,
    {ok, ok, Pid}.

handle_info(E, Pid) ->
    Pid ! E,
    {ok, Pid}.

terminate(_, _) -> ok.

code_change(_OldVsn, Pid, _Extra) ->
    {ok, Pid}.
