%%% The core of the app: the server in charge of tracking processes.
-module(regis_server).
-behaviour(gen_server).

-export([start_link/0, stop/0, register/2, unregister/1, whereis/1,
         get_names/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% We have two indexes: one by name and one by pid, for
%% MAXIMUM SPEED (not actually measured).
-record(state, {pid, name}).

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
    gen_server:call(?MODULE, {whereis, Name}).

%% Find all the names currently registered.
get_names() ->
    gen_server:call(?MODULE, get_names).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    %% Using gb_trees to store items. gb_trees generally have
    %% good overall performance.
    {ok, #state{pid = gb_trees:empty(),
                name = gb_trees:empty()}}.

handle_call({register, Name, Pid}, _From, S = #state{pid=P, name=N}) ->
    case {gb_trees:is_defined(Pid, P), gb_trees:is_defined(Name, N)} of
        {true, _} ->
            {reply, {error, already_named}, S};
        {_, true} ->
            {reply, {error, name_taken}, S};
        {false, false} ->
            Ref = erlang:monitor(process, Pid),
            {reply, ok, S#state{pid=gb_trees:insert(Pid, {Name,Ref}, P),
                                name=gb_trees:insert(Name, {Pid,Ref}, N)}}
    end;
handle_call({unregister, Name}, _From, S = #state{pid=P, name=N}) ->
    case gb_trees:lookup(Name, N) of
        {value, {Pid,Ref}} ->
            erlang:demonitor(Ref, [flush]),
            {reply, ok, S#state{pid=gb_trees:delete(Pid, P),
                                name=gb_trees:delete(Name, N)}};
        none ->
            {reply, ok, S}
    end;
handle_call({whereis, Name}, _From, S = #state{name=N}) ->
    case gb_trees:lookup(Name, N) of
        {value, {Pid,_}} ->
            {reply, Pid, S};
        none ->
            {reply, undefined, S}
    end;
handle_call(get_names, _From, S = #state{name=N}) ->
    {reply, gb_trees:keys(N), S};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, S = #state{pid=P,name=N}) ->
    {value, {Name, Ref}} = gb_trees:lookup(Pid, P),
    {noreply, S#state{pid = gb_trees:delete(Pid, P),
                 name = gb_trees:delete(Name, N)}};
handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


