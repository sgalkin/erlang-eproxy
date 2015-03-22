-module(socket_forward_handler).

-export([init/1, data/3, terminate/2]).

-record(state, {socket_a, socket_b}).

init(Socket) ->
    {ok, Local} = inet:sockname(Socket),
    {ok, Remote} = iptables:destination(Socket),

    case {Local, Remote} of
        {L, R} when L =:= R ->
            {error, {same_destination, L}};
        {_, R} ->
            connect(R, #state{socket_a=Socket})
    end.

data(Socket, Data, #state{socket_a=Socket, socket_b=B} = State) ->
    send(B, Data, State);
data(Socket, Data, #state{socket_a=A, socket_b=Socket} = State) ->
    send(A, Data, State).

terminate(_, #state{socket_a=A, socket_b=B}) ->
    gen_tcp:close(A),
    gen_tcp:close(B).

connect({Address, Port}, State) ->
    Options = [{active, once}, 
               {packet, 0},
               {keepalive, true},
               {mode, binary},
               {nodelay, true}],
    case gen_tcp:connect(Address, Port, Options) of
        {ok, Socket} ->
            {ok, State#state{socket_b=Socket}};
        {error, Reason} ->
            {error, Reason}
    end.

% TODO: use channel supervisor and passive sockets
send(Socket, Data, #state{socket_a=A, socket_b=B} = State) ->
    ok = gen_tcp:send(Socket, Data),
    ok = inet:setopts(A, [{active, once}]),
    ok = inet:setopts(B, [{active, once}]),
    State.
