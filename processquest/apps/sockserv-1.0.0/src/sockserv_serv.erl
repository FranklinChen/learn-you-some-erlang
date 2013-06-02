%%% Handles socket connections, and bridges a remote server
%%% With a progressquest game.
-module(sockserv_serv).
-behaviour(gen_server).

-record(state, {name, % player's name
                next, % next step, used when initializing
                socket}). % the current socket

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SOCK(Msg), {tcp, _Port, Msg}).
-define(TIME, 800).
-define(EXP, 50).

%% The socket is passed in from sockserv_sup.
%% It's a listen socket, as started by gen_tcp:listen/2.
%%
%% In Erlang, a TCP socket must be started as a listening socket first.
%% The listening socket can then be used to listen for a connection,
%% meant to be accepted. To do so, use gen_tcp:accept/1-2, as it is done
%% later in this module.
%%
%% A single listen socket can be used by many processes, each accepting
%% a communication. When a communication is accepted with accept/1-2,
%% a new socket, called accept socket, is returned. This accept socket
%% is the one that may be used to communicate with a client.
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

%% Accepting a connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    %% this is the socket acceptance mentioned earlier
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Remember that thou art dust, and to dust thou shalt return.
    %% We want to always keep a given number of children in this app.
    sockserv_sup:start_socket(), % a new acceptor is born, praise the lord
    send(AcceptSocket, "What's your character's name?", []),
    {noreply, S#state{socket=AcceptSocket, next=name}};
%% The player has given us his name (in handle_info)
%% so we now roll stats that might or might not satisfy
%% said player.
handle_cast(roll_stats, S = #state{socket=Socket}) ->
    Roll = pq_stats:initial_roll(),
    send(Socket,
         "Stats for your character:~n"
         "  Charisma: ~B~n"
         "  Constitution: ~B~n"
         "  Dexterity: ~B~n"
         "  Intelligence: ~B~n"
         "  Strength: ~B~n"
         "  Wisdom: ~B~n~n"
         "Do you agree to these? y/n~n",
         [Points || {_Name, Points} <- lists:sort(Roll)]),
    {noreply, S#state{next={stats, Roll}}};
%% The player has accepted the stats! Start the game!
handle_cast(stats_accepted, S = #state{name=Name, next={stats, Stats}}) ->
    processquest:start_player(Name, [{stats,Stats},{time,?TIME},
                                     {lvlexp, ?EXP}]),
    processquest:subscribe(Name, sockserv_pq_events, self()),
    {noreply, S#state{next=playing}};
%% Events coming in from process quest
%% We know this because all these events' tuples start with the
%% name of the player.
handle_cast(Event, S = #state{name=N, socket=Sock}) when element(1, Event) =:= N ->
    [case E of
       {wait, Time} -> timer:sleep(Time);
       IoList -> send(Sock, IoList, [])
     end || E <- sockserv_trans:to_str(Event)], % translate to a string
    {noreply, S}.

%% The TCP client sends the string "quit". We close the connection.
handle_info(?SOCK("quit"++_), S) ->
    processquest:stop_player(S#state.name),
    {stop, normal, S};
%% We receive a string while looking for a name -- we assume that hte
%% string is the name.
handle_info(?SOCK(Str), S = #state{next=name}) ->
    Name = line(Str),
    gen_server:cast(self(), roll_stats),
    {noreply, S#state{name=Name, next=stats}};
%% The user might or might not accept the stats we rolled in handle_cast
handle_info(?SOCK(Str), S = #state{socket=Socket, next={stats, _}}) ->
    case line(Str) of
        "y" ->
            gen_server:cast(self(), stats_accepted);
        "n" ->
            gen_server:cast(self(), roll_stats);
        _ -> % ask again because we didn't get what we wanted
            send(Socket, "Answer with y (yes) or n (no)", [])
    end,
    {noreply, S};
handle_info(?SOCK(E), S = #state{socket=Socket}) ->
    send(Socket, "Unexpected input: ~p~n", [E]),
    {noreply, S};
handle_info({tcp_closed, _Socket, _}, S) ->
    {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
    {stop, normal, S};
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, #state{socket=S}) ->
    gen_tcp:close(S);
terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

%% Send a message through a socket, then make it active again.
%% The difference between an active and a passive socket is that
%% an active socket will send incoming data as Erlang messages, while
%% passive sockets will require to be polled with gen_tcp:recv/2-3.
%%
%% Depending on the context, you might want one or the other. I chose
%% to have active sockets because they feel somewhat easier to work
%% with. However, one problem with active sockets is that all input
%% is blindly changed into messages and makes it so the Erlang VM
%% is somewhat more subject to overload. Passive sockets push this
%% responsibility to the underlying implementation and the OS and are
%% somewhat safer.
%%
%% A middle ground exists, with sockets that are 'active once'.
%% The {active, once} option (can be set with inet:setopts or
%% when creating the listen socket) makes it so only *one* message
%% will be sent in active mode, and then the socket is automatically
%% turned back to passive mode. On each message reception, we turn
%% the socket back to {active once} as to achieve rate limiting.
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

%% Let's get rid of the whitespace and ignore whatever's after.
%% makes it simpler to deal with telnet.
line(Str) ->
    hd(string:tokens(Str, "\r\n ")).
