-module(socket_worker).
-export([start/2, init/1]).

start(ListenSocket, HandleModule) ->
    {ok, spawn(?MODULE, init, [[ListenSocket, HandleModule]])}.

init([ListenSocket, HandleModule]) ->
    loop(ListenSocket, HandleModule).

loop(ListenSocket, HandleModule) ->
    {ok, ConnectedSocket} = gen_tcp:connect(ListenSocket),
    HandleModule:handle(ConnectedSocket),
    loop(ListenSocket, HandleModule).
