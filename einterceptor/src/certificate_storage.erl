-module(certificate_storage).
-export([start/0, stop/0, init/0]).
-export([by_fingerprint/1, by_destination/1, save/3]).

-record(certificate_info, {fingerprint, destination, verify_timestamp, certificate}).
-record(certificate, {certificate, private_key, private_key_der}).

-define(WAKEUP_TIMEOUT, 10000).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic, ok} = mnesia:create_table(
                     certificate_info,
                     [{type, set},
                      {disc_copies, [node()]},
                      {attributes, record_info(fields, certificate_info)},
                      {index, [destination]}]),
    mnesia:info(),
    mnesia:stop().

start() ->
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([certificate_info], ?WAKEUP_TIMEOUT),
    ok = mnesia:info().
%    save(erlang:now(), "bbb", {"Ccc",2,3}),
%    io:format("~p~n", [by_destination("bbb")]).

stop() ->
    stopped = mnesia:stop().

by_fingerprint(Fingerprint) ->
    by_key(Fingerprint).

by_destination(Destination) ->
    by_index(destination, Destination).

by_index(Index, Value) ->
    by(fun (T, V) -> mnesia:index_read(T, V, Index) end, Value).

by_key(Value) ->
    by(fun mnesia:read/2, Value).

by(Function, Criteria) ->
    {atomic, Result} = 
        mnesia:transaction(
          fun() ->
                  [ {C, PK, PKDER} || 
                      #certificate_info{
                         certificate = #certificate{
                           certificate = C,
                           private_key = PK,
                           private_key_der = PKDER}} 
                          <- Function(certificate_info, Criteria) ]
          end),
    Result.
        

save(Fingerprint, Destination, {Certificate, PrivateKey, DERPrivateKey}) ->
    {atomic, ok} = 
        mnesia:transaction(
          fun() ->
                  mnesia:write(#certificate_info{
                                  fingerprint = Fingerprint,
                                  destination = Destination,
                                  verify_timestamp = erlang:now(),
                                  certificate = #certificate{
                                                   certificate = Certificate,
                                                   private_key = PrivateKey,
                                                   private_key_der = DERPrivateKey}})
          end).

%older(Timestamp) ->
%    mnesia:transaction(
%      fun() -> mnesia:index_read(
