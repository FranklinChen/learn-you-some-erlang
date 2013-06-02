%%% The core of the app: the server in charge of tracking processes.
-module(regis_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0, stop/0, register/2, unregister/1, whereis/1,
         get_names/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% Give a name to a process
register(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

%% Remove the name from a process
unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%% Find the pid associated with a process
whereis(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Pid, _Ref}] -> Pid;
        [] -> undefined
    end.

%% Find all the names currently registered.
get_names() ->
    MatchSpec = ets:fun2ms(fun({Name, _, _}) -> Name end),
    ets:select(?MODULE, MatchSpec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
    {ok, ?MODULE}.

handle_call({register, Name, Pid}, _From, Tid) ->
    %% Neither the name or the pid can already be in the table
    %% so we match for both of them in a table-long scan using this.
    MatchSpec = ets:fun2ms(fun({N,P,_Ref}) when N==Name; P==Pid -> {N,P} end),
    case ets:select(Tid, MatchSpec) of
        [] -> % free to insert
            Ref = erlang:monitor(process, Pid),
            ets:insert(Tid, {Name, Pid, Ref}),
            {reply, ok, Tid};
        [{Name,_}|_] -> % maybe more than one result, but name matches
            {reply, {error, name_taken}, Tid};
        [{_,Pid}|_] -> % maybe more than one result, but Pid matches
            {reply, {error, already_named}, Tid}
    end;
handle_call({unregister, Name}, _From, Tid) ->
    case ets:lookup(Tid, Name) of
        [{Name,_Pid,Ref}] ->
            erlang:demonitor(Ref, [flush]),
            ets:delete(Tid, Name),
            {reply, ok, Tid};
        [] ->
            {reply, ok, Tid}
    end;
handle_call(stop, _From, Tid) ->
    %% For the sake of being synchronous and because emptying ETS
    %% tables might take a bit longer than dropping data structures
    %% held in memory, dropping the table here will be safer for
    %% tricky race conditions, especially in tests where we start/stop
    %% servers a lot. In regular code, this doesn't matter.
    ets:delete(Tid),
    {stop, normal, ok, Tid};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, Tid) ->
    ets:match_delete(Tid, {'_', '_', Ref}),
    {noreply, Tid};
handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


