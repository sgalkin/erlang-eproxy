-module(mod_eserver).
-include_lib("inets/include/httpd.hrl").

-export([do/1]).

-record(request, {host, method, uri, headers=[], body={undefined, []}}).

do(Request) ->
    Handlers = 
        [ fun get_method/2,
          fun get_uri/2,
          fun get_host/2,
          fun get_headers/2,
          fun get_body/2,
          fun validate/2],

    Req = lists:foldl(fun (M, Acc) -> process(M, Request, Acc) end, 
                      { ok, #request{_=undefined} }, Handlers),

    case Req of
        { ok, Record } ->
            case client_sup:request(
                   Record#request.host, 
                   Record#request.method, 
                   Record#request.uri,
                   Record#request.headers,
                   Record#request.body) of
                { error, Reason } ->
                    { break, [{response, {500, io_lib:format("~p",[Reason])}}] };
                { ok, Reply } ->
                    Code = element(1, Reply),
                    Headers = element(2, Reply),
                    Body = element(3, Reply),
                    H = [{code, Code} | Headers],
                    B = binary_to_list(Body),
                    { proceed, [{response,{response, H, [B]}}] };
                X ->
                    { break, [{response, {500, io_lib:format("~p",[X])}}] }
            end;
        { error, _ } ->
            { break, [{response,{400, []}}] }
    end.

process(Method, Request, Acc) ->
    case Acc of
        { ok, Record } -> Method(Request, Record);
        { error, _ } -> Acc
    end.

get_method(Request, Record) -> 
    {ok, Record#request{method=erlang:list_to_existing_atom(string:to_lower(Request#mod.method))}}.

get_uri(Request, Record) -> 
    {ok, Record#request{uri=string:strip(Request#mod.request_uri, left, $/)}}.

get_host(_Request, Record) -> 
    case http_uri:parse(Record#request.uri) of
        {ok, {_,_,Host,_,_,_}} ->
            {ok, Record#request{host = Host}};
        Error -> Error
    end.

get_headers(Request, Record) -> 
    Host = [{"host", Record#request.host}],
    Connection = [{"connection", "keep-alive"}],
    Extra = Host ++ Connection,
    Headers = lists:foldl(
                fun ({K,_},Acc) -> proplists:delete(K, Acc) end, 
                Request#mod.parsed_header,
                Extra) ++ Extra,
    {ok, Record#request{headers=Headers}}.

get_body(Request, Record) -> 
    ContentType = proplists:get_value("content-type", Request#mod.parsed_header),
    Body = Request#mod.entity_body,
    case { ContentType, Body } of
        { undefined, [_] } ->
            { error, invalid_content_type };
        _ ->
            {ok, Record#request{body={ContentType, Body}}}
    end.

validate(_Request, Record) ->
    case lists:any(fun (X) -> X == undefined end, tuple_to_list(Record)) of
        true ->
            { error, undefined_fields };
        false ->
            { ok, Record }
    end.
