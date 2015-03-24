-module(test).

-export([run/0]).

tracer() ->
    dbg:tracer(),
    dbg:p(all,c),
    dbg:tpl(pipe_sup, x),
    dbg:tpl(pipe_worker, x),
    dbg:tpl(supervisor_utils, x).

run() ->
    tracer(),
    ssl:start(),
%    eunit:test(pipe_worker).
    eunit:test(connection_serv),
    ssl:stop().

run_() ->
    tracer(),
    ssl:start(),
    %cprof:start(),
    ok = application:start(einterceptor),
    timer:sleep(60000),
    ok = application:stop(einterceptor),
    %io:format("~p~n", [cprof:analyze()]),
    %cprof:stop(),
    ssl:stop().

