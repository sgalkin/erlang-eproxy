-module(eserver_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).

start_link(_Config) ->
    supervisor:start_link(?MODULE, [_Config]).

init([Config]) ->
    io:format("~p starting ~p (~p)~n", [self(), ?MODULE, Config]),
    HttpdConfig = [{proplist_file, Config}],

    { ok, {{one_for_one, 1, 60},
           [{inets, {inets, start, [httpd, HttpdConfig]}, permanent, infinity, supervisor, [inets]},
            {clients, {client_sup, start_link, []}, permanent, infinity, supervisor, [client_sup]}]}}.
