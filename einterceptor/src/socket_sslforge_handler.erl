-module(socket_sslforge_handler).

-export([init/1, data/3, terminate/2]).
-record(state, {socket_a, socket_b}).

init(Socket) ->
    {ok, Local} = inet:sockname(Socket),
    {ok, Remote} = iptables:destination(Socket),

    case {Local, Remote} of
        {L, R} when L =:= R ->
            {error, {same_destination, L}};
        {_, R} ->
            forge_ssl(R, #state{socket_a=Socket})
    end.

data(Socket, Data, #state{socket_a=Socket, socket_b=B} = State) ->
    send(Socket, B, Data, State);
data(Socket, Data, #state{socket_a=A, socket_b=Socket} = State) ->
    send(Socket, A, Data, State).

terminate(_, #state{socket_a=A, socket_b=B}) ->
    ssl:close(A),
    ssl:close(B).

forge_ssl({Address, Port}, #state{socket_a=A} = S) ->
    SslOptions = [{active, once},
                  {packet, 0},
                  {keepalive, true},
                  {mode, binary},
                  {nodelay, true},
                  {reuse_sessions, true}],

    (Acceptor = spawn(fun() -> accept(A, Address, Port, SslOptions) end)) ! self(),
    spawn(fun() -> connect(Address, Port, SslOptions) end) ! self(),
    gen_tcp:controlling_process(A, Acceptor),
    State = loop([accept, connect], S),
    {ok, activate(State)}.

loop([], State) ->
    State;
loop(Pending, State) ->
    receive 
        {accept, Socket} ->
            loop(lists:delete(accept, Pending), State#state{socket_a=Socket});
        {connect, Socket} ->
            loop(lists:delete(connect, Pending), State#state{socket_b=Socket})
    end.

accept(ListenSocket, Address, Port, Options) ->
    {Key, Certificate} = certificate:get(Address, Port),

    ServerOptions = 
        [{versions, ['tlsv1.1', 'tlsv1.2']},
         {cert, Certificate},
         {key, {'ECPrivateKey', Key}}], % 'ECPrivateKey' no this atom in validate_option

    {ok, Socket} = 
        ssl:ssl_accept(ListenSocket, lists:append([ServerOptions, Options])),
    Caller = receive C -> C end,
    ssl:controlling_process(Socket, Caller),
    Caller ! {accept, Socket}.

connect(Address, Port, Options) ->
    {ok, Socket} = ssl:connect(Address, Port, Options),
    Caller = receive C -> C end,
    ssl:controlling_process(Socket, Caller),
    Caller ! {connect, Socket}.

% TODO: use channel supervisor and passive sockets
send(_Source, Destination, Data, State) ->
    ok = ssl:send(Destination, Data),
    activate(State).
    %ok = ssl:setopts(peer(Socket, State), [{active, once}]),
%    ok = ssl:setopts(A, [{active, once}]),
%    ok = ssl:setopts(B, [{active, once}]),

%peer(Socket, #state{socket_a=Socket, socket_b=B}) ->
%    B;
%peer(Socket, #state{socket_b=Socket, socket_a=A}) ->
%    A.

activate(#state{socket_a=A, socket_b=B} = State) ->
    ssl:setopts(A, [{active, once}]),
    ssl:setopts(B, [{active, once}]),
    State.
