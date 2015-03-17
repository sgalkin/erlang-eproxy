-module(einterceptor_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, [Port]).

init([Port | _]) ->
    Restart = {one_for_one, 1, 60},

%{Id :: child_id(),
% StartFunc :: mfargs(),
% Restart :: restart(),
% Shutdown :: shutdown(),
% Type :: worker(),
% Modules :: modules()}

    Childs = [ {fun supervisor/1, {socket_sup, start_link, [Port]}} ],
    
    {ok, {Restart, [F(X) || {F, X} <- Childs]}}.

supervisor({M, _, _} = MFA) ->
    {M, MFA, permanent, infinity, supervisor, [M]}.
