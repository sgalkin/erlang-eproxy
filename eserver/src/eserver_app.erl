-module(eserver_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    io:format("~p starting ~p (~p, ~p)~n", [self(), ?MODULE, _Type, _Args]),

    % should be started by script
    ssl:start(),
    inets:start(),

    eserver_sup:start_link(get_config(default_config())).

stop(_State) ->
    io:format("~p stopping ~p (~p)~n", [self(), ?MODULE, _State]).

application_name() ->
    { ok, Application } = application:get_application(),
    Application.

default_config() ->
    Application = application_name(),
    filename:join(
      code:lib_dir(Application, priv),
      string:concat(atom_to_list(Application), ".conf")).

get_config(Default) ->
    Candidates = [ application:get_env(application_name(), config, Default), Default ],
    hd(lists:filter(fun filelib:is_regular/1, Candidates)).
