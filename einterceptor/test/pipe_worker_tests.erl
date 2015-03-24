-module(pipe_worker_tests).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    {ok, Sup} = pipe_sup:start_link(),
    timer:sleep(500),
    ?debugFmt("~p~n", [supervisor:count_children(Sup)]),
    timer:sleep(1500),
    ?debugFmt("~p~n", [supervisor:count_children(Sup)]).


%    ?assert(supervisor:count_children(Sup) == 1).

