-module(certificate).

-export([get/2, fingerprint/1]).

-include_lib("public_key/include/public_key.hrl").

fingerprint(Certificate) ->
    crypto:hash(sha, Certificate).

get(Address, Port) ->
    case certificate_storage:by_destination({Address, Port}) of
        [{Certificate, _, PrivateKeyDer}] -> {PrivateKeyDer, Certificate};
        _ ->
            % TODO: handle more than 1 by destination
            forge_certificate(Address, Port)
    end.

forge_certificate(Address, Port) ->
    OriginalCertificate = fetch_certificate(Address, Port),
    OriginalFingerprint = fingerprint(OriginalCertificate),

    CertificatInfo = 
        case certificate_storage:by_fingerprint(OriginalFingerprint) of
            [] -> forge_certificate(OriginalCertificate);
            CI -> CI
        end,

    certificate_storage:save(OriginalFingerprint, {Address, Port}, CertificatInfo),
    {ForgedCertificate, _, ForgedPrivateKeyDer} = CertificatInfo,
    {ForgedPrivateKeyDer, ForgedCertificate}.
            
forge_certificate(OriginalCertificate) ->
    PrivateKey = key_server:private_key(),
    PublicKey = key_server:public_key(PrivateKey),
    ForgedCertificate = sign_server:sign(OriginalCertificate, PublicKey),
    {ForgedCertificate, PrivateKey, key_server:private_key_der(PrivateKey)}.
    
fetch_certificate(Address, Port) ->
    SslOptions = [],
    % TODO abort after get cert don't complete handshake
    {ok, Socket} = ssl:connect(Address, Port, SslOptions),
    {ok, Certificate} = ssl:peercert(Socket),
    ok = ssl:close(Socket),
    Certificate.
