-module(einterceptor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    io:format("~p~n", [application:get_all_env(einterceptor)]),

    {ok, Port} = application:get_env(einterceptor, port),

    {ok, CACertificate} = application:get_env(einterceptor, cacert),
    {ok, CAKey} = application:get_env(einterceptor, cakey),

    {ok, ForgeKey} = application:get_env(einterceptor, forgekey),

    certificate_storage:start(),

    Supervisors = [{socket_sup, start_link, [Port]}],
    Workers = [{sign_server, start_link, [CAKey, CACertificate]},
               {key_server, start_link, [ForgeKey]}],
  
    Restart = {one_for_one, 1, 60},

%{Id :: child_id(),
% StartFunc :: mfargs(),
% Restart :: restart(),
% Shutdown :: shutdown(),
% Type :: worker(),
% Modules :: modules()}

    Childs = lists:append([supervisor_utils:worker(X, permanent, brutal_kill) || X <- Workers],
                          [supervisor_utils:supervisor(X, permanent) || X <- Supervisors]),
    {ok, {Restart, Childs}}.
