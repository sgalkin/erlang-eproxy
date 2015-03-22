-module(einterceptor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> % can return State
    %dbg:tracer(),
    %dbg:p(all,c),
    %dbg:tpl(einterceptor_app, x),
    %dbg:tpl(einterceptor_sup, x),
    %dbg:tpl(socket_sup, x),
    %dbg:tpl(socket_worker, x),
    %dbg:tpl(socket_httpsforge_handler, x),
    %dbg:tpl(certificate, x),
    %dbg:tpl(sign_sup, x),
    %dbg:tpl(sign_server, x),

    einterceptor_sup:start_link().

stop(_State) ->
    ok.
