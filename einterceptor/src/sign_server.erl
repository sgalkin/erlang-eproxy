-module(sign_server).
-behaviour(gen_server).

-include_lib("public_key/include/public_key.hrl").

-export([start_link/2, sign/2]).
-export([init/1, handle_call/3]).
-export([handle_info/2, handle_cast/2, terminate/2, code_change/3]). % stubs

-record(state, {key, issuer, signature}).

-define(UNWANTED_EXTENSIONS, 
        [{2,5,29,14},
         {2,5,29,35},
         {2,5,29,31},
         {1,3,6,1,5,5,7,1,1}]).

handle_info(_, S) -> {noreply, S}.
handle_cast(_, S) -> {noreply, S}.
terminate(_, _) -> ok.
code_change(_, S, _) -> {ok, S}.

start_link(CAKey, CACertificate) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CAKey, CACertificate], []).

sign(#'OTPTBSCertificate'{} = TBSCertificate, PublicKey) ->
    gen_server:call(?MODULE, {sign, TBSCertificate, PublicKey});
sign(Certificate, PublicKey) ->
    #'OTPCertificate'{tbsCertificate = TBSCertificate} = 
        public_key:pkix_decode_cert(Certificate, otp),
    sign(TBSCertificate, PublicKey).

init([CAKey, CACertificate]) ->
    {ok, PEMKey} = file:read_file(CAKey),
    [_, DERKey] = public_key:pem_decode(PEMKey),
    Key = public_key:pem_entry_decode(DERKey),

    {ok, PEMCertificate} = file:read_file(CACertificate),
    [{_,DERCertificate,not_encrypted}] = public_key:pem_decode(PEMCertificate),
    Certificate = public_key:pkix_decode_cert(DERCertificate, otp),

    {ok, #state{key=Key, issuer=issuer(Certificate), signature=signature_algorithm()}}.

handle_call({sign,
             #'OTPTBSCertificate'{extensions=Extensions}=Certificate,PublicKey},
            _,
            #state{key=Key,issuer=Issuer,signature=Signature}=State) ->
    Request = Certificate#'OTPTBSCertificate'{
                issuer=Issuer, 
                signature=Signature,
                subjectPublicKeyInfo=public_key_info(PublicKey),
                extensions=filter_extensions(Extensions)},
    SignedRequest = public_key:pkix_sign(Request, Key),
    { reply, SignedRequest, State }.

filter_extensions(Extensions) ->
    lists:filter(
      fun ({_, K, _, _}) ->
              not sets:is_element(K, sets:from_list(?UNWANTED_EXTENSIONS)) end,
      Extensions). 

public_key_info({Key, Parameters}) ->
    #'OTPSubjectPublicKeyInfo'{
       algorithm = #'PublicKeyAlgorithm'{
                      algorithm = ?'id-ecPublicKey',
                      parameters = Parameters},
       subjectPublicKey = #'ECPoint'{point = Key}}.

signature_algorithm() ->
    #'SignatureAlgorithm'{
       algorithm = ?'ecdsa-with-SHA256',
       parameters = 'NULL'
      }.

issuer(#'OTPCertificate'{tbsCertificate=TBSCertificate}=_) ->
    TBSCertificate#'OTPTBSCertificate'.issuer.
