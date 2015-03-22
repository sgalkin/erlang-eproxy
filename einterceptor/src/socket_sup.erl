-module(socket_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-export([start_worker/0]).

-define(WORKERS, 4).
-define(TERMINATE_TIMEOUT, 1000).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    {ok, ListenSocket} = 
        gen_tcp:listen(Port, 
                       [{active, false},
                        {packet, 0},
                        {backlog, 1024},
                        {keepalive, true},
                        {mode, binary},
                        {nodelay, true},
                        {reuseaddr, true}]),

    Restart = {simple_one_for_one, 60, 3600},
    Childs = [{fun worker/1, 
               {socket_worker, 
                start_link, 
                [ListenSocket, ?MODULE, socket_sslforge_handler]}}],
    spawn(fun init_workers/0),

    {ok, {Restart, [F(X) || {F, X} <- Childs]}}.

worker({M, _, _} = MFA) ->
    {M, MFA, temporary, ?TERMINATE_TIMEOUT, worker, [M]}.

start_worker() ->
    supervisor:start_child(?MODULE, []),
    io:format("~p: ~p~n", [?MODULE, supervisor:count_children(?MODULE)]).

init_workers() ->
    [ start_worker() || _ <- lists:seq(1, ?WORKERS) ],
    ok.
