-module(einterceptor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> % can return State
    dbg:tracer(),
    dbg:p(all,c),
    %dbg:tpl(einterceptor_app, x),
    %dbg:tpl(einterceptor_sup, x),
    %dbg:tpl(socket_sup, x),
    dbg:tpl(socket_worker, x),
    dbg:tpl(socket_forward_handler,x),

    {ok, Port} = application:get_env(port),
    {IPort, []} = string:to_integer(Port),
    einterceptor_sup:start_link(IPort).

stop(_State) ->
    ok.
