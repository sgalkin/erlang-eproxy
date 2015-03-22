-module(socket_httpsforge_handler).

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
    send(B, Data, State);
data(Socket, Data, #state{socket_a=A, socket_b=Socket} = State) ->
    send(A, Data, State).

terminate(_, #state{socket_a=A, socket_b=B}) ->
    ssl:close(A),
    ssl:close(B).

forge_ssl({Address, Port}, #state{socket_a=A} = State) ->
    {Key, Certificate} = certificate:get(Address, Port),
    Options = [{active, once},
               {packet, 0},
               {keepalive, true},
               {mode, binary},
               {nodelay, true}],
    SslOptions = 
        [{cert, Certificate},
         {key, {'ECPrivateKey', Key}}, % 'ECPrivateKey' no this atom in validate_option
         {versions, ['tlsv1.2']},
         {reuse_sessions, true}],
    inet:setopts(A, [{active, false}]),
    {ok, SslSocket} = ssl:ssl_accept(A, lists:append([SslOptions, Options])),
    ssl:setopts(SslSocket, [{active, once}]),

    case ssl:connect(Address, Port, Options) of
        {ok, Socket} ->
            {ok, State#state{socket_a=SslSocket, socket_b=Socket}};
        {error, Reason} ->
            {error, Reason}
    end.

% TODO: use channel supervisor and passive sockets
send(Socket, Data, #state{socket_a=A, socket_b=B} = State) ->
    ok = ssl:send(Socket, Data),
    %ok = ssl:setopts(peer(Socket, State), [{active, once}]),
    ok = ssl:setopts(A, [{active, once}]),
    ok = ssl:setopts(B, [{active, once}]),
    State.

peer(Socket, #state{socket_a=Socket, socket_b=B}) ->
    B;
peer(Socket, #state{socket_b=Socket, socket_a=A}) ->
    A.
