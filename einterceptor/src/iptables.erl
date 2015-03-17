-module(iptables).

-export([destination/1]).

-define(AF_INET, 2).
-define(AF_INET6, 10).
-define(SOL_IP, 0).
-define(SOL_IPV6, 41).
-define(SO_ORIGINAL_DST, 80).
-define(SO_ORIGINAL_DST_LEN, 32).

read_ipv4(IP) ->
    {(IP bsr 24) band 16#ff,
     (IP bsr 16) band 16#ff,
     (IP bsr 8) band 16#ff,
     (IP bsr 0) band 16#ff}.

read_sockaddr(<<?AF_INET:16/native-unsigned-integer,
                Port:16/unsigned-integer,
                IP:32/integer,
                _/binary>>) ->
    { read_ipv4(IP), Port }.

destination(Socket) ->
    {ok, 
     [{raw, _, _, R}]} =
        inet:getopts(Socket, [{raw, ?SOL_IP, ?SO_ORIGINAL_DST, ?SO_ORIGINAL_DST_LEN}]),
    {ok, read_sockaddr(R)}.
