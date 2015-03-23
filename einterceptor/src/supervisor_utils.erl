-module(supervisor_utils).

-export([supervisor/2, worker/3]).

supervisor(MFA, Type) ->
    child_spec(MFA, Type, infinity, supervisor).

worker(MFA, Type, Timeout) ->
    child_spec(MFA, Type, Timeout, worker).

child_spec({M, _, _} = MFA, Type, Timeout, Kind) ->
    {M, MFA, Type, Timeout, Kind, [M]}.
