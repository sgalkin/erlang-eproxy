-module(supervisor_utils).

-export([supervisor/2, worker/3, init_workers/1]).

supervisor(MFA, Type) ->
    child_spec(MFA, Type, infinity, supervisor).

worker(MFA, Type, Timeout) ->
    child_spec(MFA, Type, Timeout, worker).

child_spec({M, _, _} = MFA, Type, Timeout, Kind) ->
    {M, MFA, Type, Timeout, Kind, [M]}.

init_workers(Number) ->
    Caller = self(),
    spawn(fun () -> init_workers(Caller, Number) end).

init_workers(Supervisor, Number) ->
    [supervisor:start_child(Supervisor, []) || _ <- lists:seq(1, Number)],
    ok.

