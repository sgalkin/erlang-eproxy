-module(pipe_worker).

-export([start_link/0, init/0]).

start_link() ->
    {ok, spawn(?MODULE, init, [])}.

init() ->
    timer:sleep(1000),
    ok.

