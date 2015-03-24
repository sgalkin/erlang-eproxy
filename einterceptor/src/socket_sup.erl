-module(socket_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(WORKERS, 16).
-define(TCP_OPTIONS,
        [{active, false},
         {packet, 0},
         {backlog, 1024},
         {keepalive, true},
         {mode, binary},
         {nodelay, true},
         {reuseaddr, true}]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),

    Restart = {simple_one_for_one, 60, 3600},
    Childs = [{socket_worker, 
               start, 
               [ListenSocket, socket_forward_handler]}],
    
    supervisor_utils:init_workers(?WORKERS),

    {ok, {Restart, 
          [supervisor_utils:worker(X, permanent, brutal_kill) || X <- Childs]}}.

