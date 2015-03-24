-module(pipe_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(%{local, ?MODULE}, 
      ?MODULE, []).

init([]) ->
    Restart = {simple_one_for_one, 60, 3600},
    Childs = [{pipe_worker, start_link, []}],

    supervisor_utils:init_workers(1),

    {ok, {Restart, 
          [supervisor_utils:worker(X, permanent, brutal_kill) || X <- Childs]}}.
