-module(client_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).
-export([request/5]).

-define(CLIENT_KILL_TIMEOUT, 5000).
-define(WORKER_EXPONENT, 0).
-define(WORKERS_MASK, bnot (-1 bsl ?WORKER_EXPONENT)).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    io:format("~p starting ~p ~n", [self(), ?MODULE]),
    spawn_link(fun init_clients/0),

    { ok, {{simple_one_for_one, 1, 60},
           [{client, {client_server, start_link, []}, permanent, ?CLIENT_KILL_TIMEOUT, worker, [client_server]}] }}.

init_clients() ->
    [ client_server:start_link(I) || I <- lists:seq(0, ?WORKERS_MASK) ]. 

request(Host, Method, Uri, Headers, Body) ->
    Id = erlang:crc32(Host) band ?WORKERS_MASK,
    client_server:request(Id, Method, Uri, Headers, Body).
