-module(key_server).
-behaviour(gen_server).

-include_lib("public_key/include/public_key.hrl").

-export([start_link/1, private_key/0, private_key_der/1, public_key/1]).
-export([init/1, handle_call/3]).
-export([handle_info/2, handle_cast/2, terminate/2, code_change/3]). % stubs

-record(state, {private_key, private_key_der, public_key}).

start_link(Key) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Key], []).

private_key() ->
    gen_server:call(?MODULE, private_key).

private_key_der(PrivateKey) ->
    gen_server:call(?MODULE, {private_key_der, PrivateKey}).
   
public_key(PrivateKey) ->
    gen_server:call(?MODULE, {public_key, PrivateKey}).

handle_info(_, S) -> {noreply, S}.
handle_cast(_, S) -> {noreply, S}.
terminate(_, _) -> ok.
code_change(_, S, _) -> {ok, S}.

init([Key]) ->
    {ok, PEMPrivateKey} = file:read_file(Key),
    [_, {_,DERPrivateKey,_} = Entry] = public_key:pem_decode(PEMPrivateKey),
    PrivateKey = public_key:pem_entry_decode(Entry),
    PublicKey = decode_public_key(PrivateKey),
    {ok, #state{
            private_key=PrivateKey, 
            private_key_der=DERPrivateKey,
            public_key=PublicKey}}.

decode_public_key(#'ECPrivateKey'{
                     version = _Version,
                     privateKey = _PrivateKey,
                     parameters = Parameters,
                     publicKey = {0, PublicKey}}) ->
    {PublicKey, Parameters}.   

handle_call(private_key, _From, #state{private_key=PrivateKey} = State) ->
%    public_key:generate_key({namedCurve, ?secp256r1}).
    {reply, PrivateKey, State};

handle_call({private_key_der, PrivateKey}, 
            _From, 
            #state{private_key=PrivateKey, private_key_der=DERPrivateKey} = State) ->
%    public_key:der_encode('ECPrivateKey', PrivateKey).
    {reply, DERPrivateKey, State};

handle_call({public_key, PrivateKey},
            _From,
            #state{private_key=PrivateKey, public_key=PublicKey} = State) ->
    %decode_public_key(PrivateKey).
    {reply, PublicKey, State}.
