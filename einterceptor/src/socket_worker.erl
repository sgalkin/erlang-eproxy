-module(socket_worker).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {supervisor, handler, state}).

start_link(Socket, Supervisor, Handler) ->
    gen_server:start_link(?MODULE, [Socket, Supervisor, Handler], []).

init([Socket, Supervisor, Handler]) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), {accept, Socket}),
    {ok, #state{supervisor=Supervisor, handler=Handler}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

terminate(shutdown, #state{handler=Handler, state=State}) ->
    Handler:terminate(shutdown, State);
terminate(normal, _) ->
    ok.

handle_cast({accept, ListenSocket}, 
            State=#state{supervisor=Supervisor, handler=Handler}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    Supervisor:start_worker(),

    case Handler:init(AcceptSocket) of
        {ok, HState} ->
            {noreply, State#state{state=HState}};
        {error, _} ->
            {stop, normal, State}
    end.

% Packets can be sent to the returned socket Socket using send/2. 
% Packets sent from the peer are delivered as messages:
%              {tcp, Socket, Data}
% If the socket is closed, the following message is delivered:
%              {tcp_closed, Socket}
% If an error occurs on the socket, the following message is delivered:
%              {tcp_error, Socket, Reason}
handle_info({T, Socket, Data}, #state{handler=Handler, state=HState} = State) 
  when T == tcp; T == ssl ->
    {noreply, State#state{state=Handler:data(Socket, Data, HState)}};

handle_info({T, _S}, State) 
  when T == tcp_closed; T == ssl_closed ->
    {stop, normal, State};

handle_info({T, _S, _}, State)
  when T == tcp_error; T == ssl_error ->
    {stop, normal, State}.
