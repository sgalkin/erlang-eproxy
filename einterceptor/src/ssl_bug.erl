-module(ssl_bug).
-export([repro/0]).

-include_lib("public_key/include/public_key.hrl").

repro() ->
    ssl:start(),
    sign_server:start_link("/home/sergo/stuff/erlang-eproxy/ca.key",
                           "/home/sergo/stuff/erlang-eproxy/ca.pem"),
    {ok, PEMCertificate} = file:read_file("/home/sergo/stuff/erlang-eproxy/ca.pem"),
    [{_,DERCertificate,not_encrypted}] = public_key:pem_decode(PEMCertificate),
    Certificate = public_key:pkix_decode_cert(DERCertificate, otp),

%-record('PrivateKeyInfo',{
%version, privateKeyAlgorithm, privateKey, attributes = asn1_NOVALUE}).

%-record('PrivateKeyInfo_privateKeyAlgorithm',{
%algorithm, parameters = asn1_NOVALUE}).

%-record('ECPrivateKey',{
%version, privateKey, parameters = asn1_NOVALUE, publicKey = asn1_NOVALUE}).

%       algorithm = #'PublicKeyAlgorithm'{
%                      algorithm = ?'id-ecPublicKey',
%                      parameters = Params},
%       subjectPublicKey = #'ECPoint'{point = PublicKey}}.
%encryptionAlgorithm

    %dbg:tracer(),
    %dbg:p(all,c),
    %dbg:tpl(ssl, x),
    %dbg:tpl(ssl_socket, x),

    {ok,PEMMMM} = file:read_file("/home/sergo/stuff/erlang-eproxy/einterceptor/p8file.pem"),
    [{_,DERRR,_}] = public_key:pem_decode(PEMMMM),
    DDDD = public_key:der_decode('PrivateKeyInfo', DERRR),
    io:format("~p~n", [DDDD]),

    {ok,PEM} = file:read_file("/home/sergo/stuff/erlang-eproxy/ca.key"),
    [{_,PAR,_}, {_,DER,_}] = public_key:pem_decode(PEM),
    %DEC = public_key:pem_entry_decode(DER),
%    DECPAR = public_key:pem_entry_decode(PAR),
    %io:format("DER: ~p~nDEC: ~p~n~p~n~p~n", [DER, DEC, PAR, ""]),
    
    PKI = #'PrivateKeyInfo'{
             version = v1,
             privateKeyAlgorithm = #'PrivateKeyInfo_privateKeyAlgorithm'{
                                      algorithm = ?'id-ecPublicKey',
                                      %algorithm = ?'ecdsa-with-SHA256',
                                      %algorithm = ?'secp256r1'
                                      parameters = PAR
                                     },
             privateKey = DER},

    PKIDER = public_key:der_encode('PrivateKeyInfo', PKI),
    io:format("DER: ~p~nDEC: ~p~n", [PKIDER, PKI]),

    %{  Key, Certificate } = certificate:get({93,184,216,34}, 443),
    SslOptions = 
        [{cert, DERCertificate},
         %{key, {'PrivateKeyInfo',PKIDER}},
         {key, {'ECPrivateKey', DER}},
         %{certfile, "/home/sergo/stuff/erlang-eproxy/ca.pem"},
         %{keyfile, "/home/sergo/stuff/erlang-eproxy/ca.key"},
         %{keyfile, "/home/sergo/stuff/erlang-eproxy/einterceptor/p8file.pem"},
         {versions, ['tlsv1.2']},
         {reuse_sessions, true}],
    {ok,R} = ssl:listen(9000, SslOptions),
    {ok,TA} = ssl:transport_accept(R),
    A = ssl:ssl_accept(TA),
    io:format("Sok: ~p~n", [A]).
