-module(connection_serv_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-define(INACTIVITY_TIMEOUT, 100).

-record(state, {sut, socket}).

example() ->
    {ok, Example} = inet:gethostbyname("example.com", inet),
    hd(Example#hostent.h_addr_list).

connection_serv_fixture_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun get_connection/1,
      fun put_connection/1,
      fun inactivity/1,
      fun socket_messages/1
     ]}.

setup() ->
    ssl:start(),
    {ok, SUT} = connection_serv:start_link(
                  [{timeout, [{inactivity, ?INACTIVITY_TIMEOUT}]}]),

    Address = {127,0,0,1},
    Port = 9000,
    {ok, SSocket} = gen_tcp:listen(Port, [{ip, Address}, {reuseaddr, true}]),

    #state{sut=SUT, socket=SSocket}.

cleanup(#state{sut=SUT,socket=SSocket}=_) ->
    exit(SUT, normal),
    gen_tcp:close(SSocket),
    ssl:stop().

get_connection(_State) ->
    Address = example(),
    [fun() ->
             Socket = connection_serv:Function(Address, Port),
             ?assert(Module:peername(Socket) == {ok, {Address, Port}})
     end || 
        {Module, Function, Port} <- [{inet, get_tcp_connection, 80},
                                     {ssl, get_ssl_connection, 443}]].

put_connection(_State) ->
    Address = example(),
    [fun() ->
             {ok, Socket} = Module:connect(Address, Port, []),
             connection_serv:PutFunction(Socket),
             S1 = connection_serv:GetFunction(Address, Port),
             S2 = connection_serv:GetFunction(Address, Port),
             ?assert(S1 =:= Socket),
             ?assert(S2 =/= Socket)
     end || {Module, PutFunction, GetFunction, Port} <-
                [{gen_tcp, put_tcp_connection, get_tcp_connection, 80},
                 {ssl, put_ssl_connection, get_ssl_connection, 443}]].

inactivity(_State) ->
    Address = example(),
    [fun() ->
             {ok, Socket} = Module:connect(Address, Port, []),
             connection_serv:PutFunction(Socket),
             timer:sleep(2 * ?INACTIVITY_TIMEOUT),
             S = connection_serv:GetFunction(Address, Port),
             ?assert(S =/= Socket)
     end || {Module, PutFunction, GetFunction, Port} <-
                [{gen_tcp, put_tcp_connection, get_tcp_connection, 80},
                 {ssl, put_ssl_connection, get_ssl_connection, 443}]].

socket_messages(#state{socket=SSocket}=_) ->
    [fun() ->
             {ok, {Address, Port}} = inet:sockname(SSocket),
             S1 = connection_serv:get_tcp_connection(Address, Port),
             {ok, CSocket} = gen_tcp:accept(SSocket),
             connection_serv:put_tcp_connection(S1),
             timer:sleep(50),
             ok = SocketFun(CSocket),
             timer:sleep(50),
             S2 = connection_serv:get_tcp_connection(Address, Port),
             gen_tcp:accept(SSocket),
             ?assert(S2 =/= S1)
     end || SocketFun <- [ fun(S) -> gen_tcp:send(S, <<1>>) end,
                           fun(S) -> gen_tcp:close(S) end]].
