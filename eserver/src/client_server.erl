-module(client_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([request/5, response/2]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {id, name, pending=dict:new()}).

request(Id, Method, Uri, Headers, Body) ->
    gen_server:call(process_name(Id), {request, Method, Uri, Headers, Body}).

response(Id, Response) ->
    gen_server:cast(process_name(Id), { response, Response }).

start_link(Id) ->
%    dbg:tracer(),dbg:p(all,c),dbg:tpl(client_server, x),
    gen_server:start_link({local, process_name(Id)}, ?MODULE, [Id], [{debug, [log]}]).

init([Id]) ->
    io:format("~p starting ~p(~p)~n", [self(), ?MODULE, Id]),
    %process_flag(trap_exit, true), % we want a cleanup callback TODO find the reason

    State = #state{id = Id, name = process_name(Id)},
    ok = create_httpc(State#state.name),
    { ok, State }.

terminate(shutdown, State) ->
    io:format("~p terniated ~p~n", [self(), State]),
    delete_httpc(State#state.name),
    ok;
terminate(_, _State) ->
    ok.

handle_call({request, Method, Uri, Headers, Body}, From, State) ->
    io:format("handle call request(~p, ~p, ~p, <body>)~nfrom ~p~n~p~n", 
              [Method, Uri, Headers, From, State]),
    %RequestHttpOptions = 
    %    [% TODO{essl, }, 
    %     {version, "HTTP/1.1"} ], % to set from request, useless!
    RequestOptions =
        [{sync, false},
         {body_format, string},
         {full_result, true},
         {headers_as_is, true},
         {socket_opts, [{keepalive, true}, {nodelay, true}]},
         {receiver, fun (Reply) -> response(State#state.id, Reply) end}],
    Profile = State#state.name,
    Request = fun (Req) -> httpc:request(Method, Req, [], RequestOptions, Profile) end,
    Result = case Body of
                 { undefined, _ } -> Request({Uri, Headers});
                 { ContentType, Body } -> Request({Uri, Headers, ContentType, Body})
             end,
    case Result of
        {ok, RequestId} ->
            Pending = State#state.pending,
            { noreply, State#state{pending = dict:store(RequestId, From, Pending)}};
        {error, Reason} ->
            { reply, {error, Reason}, State }
    end.

handle_cast({response, {RequestId, Result}}, State) ->
    io:format("handle cast response(~p, <result>) ~p~n", [RequestId, State]),
    
    Pending = State#state.pending,
    Client = dict:find(RequestId, Pending),
    NewState = State#state{pending = dict:erase(RequestId, Pending)},

    case {Client, Result} of
        {error, _} ->
            { stop, unexpected_response };
        {{ok, C}, { error, Reason } } ->
            gen_server:reply(C, {error, Reason}),
            { noreply, NewState };
        {{ok, C}, Reply} ->
            gen_server:reply(C, {ok, prepare_reply(Reply)}),
            { noreply, NewState }
    end.

handle_info(Info, State) ->
    io:format("got ~p at ~p~n", [Info, State]),
    { noreply, State }.

code_change(_, State, _) ->
     { ok, State }.

process_name(Id) ->
    % TODO fix 255 limitation
    Module = erlang:atom_to_list(?MODULE),
    Instance = Module ++ "_" ++ [Id],
    erlang:list_to_atom(Instance).

create_httpc(Profile) ->
    {ok, _} = inets:start(httpc, [{profile, Profile}]),
    Options = 
        [{max_sessions, 8}, % Maximum number of persistent connections to a host
         {max_keep_alive_length, 16}, % Maximum number of outstanding requests on the same connection to a host
         {keep_alive_timeout, 600000}, % If a persistent connection is idle longer than the keep_alive_timeout in milliseconds, the client will close the connection
         {socket_opts, [{keepalive, true}, {nodelay, true}]}],
    httpc:set_options(Options, Profile).

delete_httpc(Profile) ->
    ok = inets:stop(httpc, Profile).

prepare_reply(Response) ->
    Status = element(2, element(1, Response)),
    Headers = lists:foldl(
            fun (H, Acc) -> proplists:delete(H, Acc) end, 
            element(2, Response), 
            ["connection", "alternate-protocol"]) ++ [{"connection", "keep-alive"}],
    Body = element(3, Response),
%    io:format("~p~n", [{ Status, Headers, Body }]),
    { Status, Headers, Body }.

%sketch() ->
%    inets:start(),
%    {ok, R} = start_link(0),
%    io:format("~p~n", [R]),
%%request(Id, Method, Uri, Headers, Body) ->
%    Hdr = [{"user-agent", "foo/1.2.3.4"}, {"host", "google.com"}, {"accpet", "*/*"}],
%    Req = fun (X) ->
%                  io:format("sending request ~p~n", [X]),
%                  Resp = request(0, get, X, Hdr, {undefined, []}),
%                  io:format("~p~n", [Resp])
%          end,
%    spawn_link(fun () -> Req("http://google.com/?q=foo") end),
%    spawn_link(fun () -> Req("http://google.com/?q=bar") end),
%    timer:sleep(10000).

bar(R) ->
    io:format("~p~n", [R]).

foo() ->
    dbg:tracer(),dbg:p(all,c),dbg:tpl(httpc, x),

    ssl:start(),
    inets:start(),
    inets:start(httpc, [{profile, foo}]),
    RequestOptions = [
        %[{sync, true},
        % {body_format, string},
        % {full_result, true},
        % {headers_as_is, true}%,
         %{socket_opts, [{keepalive, true}, {nodelay, true}]}%,
        % {receiver, fun bar/1}
        ],

    H =          [],
                 % {"cookie",
           %"csm-hit=1PM0A4MJ1EKAXM90EBE8+s-1Q64C73BWF174NP7WWCG|1426443314899"},
          %{"accept-language","en-US,en;q=0.8,ru;q=0.6"},
          %{"accept-encoding","gzip, deflate, sdch"},
          %{"user-agent",
          % "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/40.0.2214.115 Safari/537.36"},
          %{"accept",
          % "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
          %{"cache-control","max-age=0"},
          %{"connection","keep-alive"},
          %{"host","example.com"}],

    R = httpc:request(get, {"https://example.com",H}, [], RequestOptions, foo),
    io:format("~p~n", [R]),
    timer:sleep(6000).

