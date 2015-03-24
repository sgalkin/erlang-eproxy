-module(connection_serv).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([get_tcp_connection/2, get_ssl_connection/2]).
-export([put_tcp_connection/1, put_ssl_connection/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([code_change/3]).

-record(state, {connections=dict:new(),
                tcp_options,
                ssl_options,
                timeout_options}).

-define(DEFAULT_TCP_OPTIONS,
        [{active, once},
         {keepalive, true},
         {mode, binary},
         {nodelay, true},
         {packet, 0}]).

-define(DEFAULT_SSL_OPTIONS, 
        ?DEFAULT_TCP_OPTIONS).

-define(DEFAULT_TIMEOUT_OPTIONS, 
        [{inactivity, 300000}]).

start_link() ->
    start_link([]).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

init([Options]) ->
    process_flag(trap_exit, true),
    TcpOptions = proplists:get_value(tcp, Options, ?DEFAULT_TCP_OPTIONS),
    SslOptions = proplists:get_value(ssl, Options, ?DEFAULT_SSL_OPTIONS),
    TimeoutOptions = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT_OPTIONS),

    {ok, #state{tcp_options=TcpOptions, 
                ssl_options=SslOptions,
                timeout_options=TimeoutOptions}}.

get_tcp_connection(Address, Port) ->
    gen_server:call(?MODULE, {get_connection, tcp, Address, Port}).
get_ssl_connection(Address, Port) ->
    gen_server:call(?MODULE, {get_connection, ssl, Address, Port}).

put_tcp_connection(Socket) ->
    ok = control(tcp, Socket, whereis(?MODULE)),
    gen_server:cast(?MODULE, {put_connection, tcp, Socket}).
put_ssl_connection(Socket) ->
    ok = control(ssl, Socket, whereis(?MODULE)),
    gen_server:cast(?MODULE, {put_connection, ssl, Socket}).

handle_call({get_connection, Type, Address, Port}, 
            {Pid, _}=_, 
            #state{connections=C}=State) ->
    Key = {Type, Address, Port},
    {Socket, Connections} = 
        case dict:find(Key, C) of
            error ->
                {connect(Type, Address, Port, State), C};
            {ok, Pool} ->
                fetch(Key, Pool, C)
        end,
    ok = control(Type, Socket, Pid),
    {reply, Socket, State#state{connections=Connections}}.

handle_cast({put_connection, Type, Socket}, 
            #state{connections=C, timeout_options=Options} = State) ->
    Key = key(Type, Socket),
    activate(Type, Socket),
    Timer = inactivity(Key, Socket, Options),
    {noreply, State#state{connections=dict:append(Key, {Socket, Timer}, C)}}.

handle_info({timeout, _Timer, {{Type,_,_} = Key, Socket}}, State) ->
    eject(Type, Socket, Key, State);
handle_info({Type, Socket, _}, State) -> % unexpected data
    eject(Type, Socket, key(Type, Socket), State);
handle_info({tcp_closed, Socket}, #state{connections=C}=State) ->
    eject(tcp, Socket, key(closed, Socket, C), State);
handle_info({ssl_closed, Socket}, #state{connections=C}=State) ->
    eject(ssl, Socket, key(closed, Socket, C), State).

terminate(_Reason, #state{connections=C}=_) ->
    [close(Type, Socket, Timer) || {{Type,_,_}, {Socket, Timer}} <- dict:to_list(C)].

code_change(_, _, _) ->
    ok.

socket_module(tcp) -> gen_tcp;
socket_module(ssl) -> ssl.

info_module(tcp) -> inet;
info_module(ssl) -> ssl.

connect(Type, Address, Port, #state{tcp_options=Options}=_) ->
    Module = socket_module(Type),
    {ok, Socket} = Module:connect(Address, Port, Options),
    Socket.

control(Type, Socket, To) ->
    Module = socket_module(Type),
    Module:controlling_process(Socket, To).

close(Type, Socket) ->
    Module = socket_module(Type),
    Module:close(Socket).

close(Type, Socket, Timer) ->
    close(Type, Socket),
    erlang:cancel_timer(Timer),
    ok.

activate(Type, Socket) ->
    Module = info_module(Type),
    ok = Module:setopts(Socket, [{active, once}]).

key(closed, Socket, Connections) ->
    hd([Key || 
           {Key, Values} <- dict:to_list(Connections), 
           {S, _} <- Values,
           S == Socket]).

key(Type, Socket) ->
    Module = info_module(Type),
    {ok, {Address, Port}} = Module:peername(Socket),
    {Type, Address, Port}.

eject(Type, Socket, Key, #state{connections=C}=State) ->
    {noreply, State#state{connections=eject(Type, Socket, Key, C)}};
eject(Type, Socket, Key, Connections) ->
    {Close, Pass} = lists:partition(fun({S,_}) -> S == Socket end, dict:fetch(Key, Connections)),
    [close(Type, S, T) || {S, T} <- Close],
    update_dict(Key, Pass, Connections).

fetch(Key, Pool, Dict) ->
    [{Socket, Timer} | Rest] = Pool,
    erlang:cancel_timer(Timer),
    {Socket, update_dict(Key, Rest, Dict)}.

update_dict(Key, [], Dict) ->
    dict:erase(Key, Dict);
update_dict(Key, Pool, Dict) ->
    dict:store(Key, Pool, Dict).

inactivity(Key, Socket, Options) ->
    erlang:start_timer(proplists:get_value(inactivity, Options), 
                       self(),
                       {Key, Socket}).
