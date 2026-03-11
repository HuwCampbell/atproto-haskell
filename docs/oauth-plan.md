OAuth Client Plan: atproto-haskell-oauth
=========================================

This document describes what an implementation of @atproto/oauth-client
would require in Haskell.  The plan is grounded in the reference TypeScript
implementation, RFC 9449 (DPoP), RFC 9126 (PAR), OAuth 2.1 (draft), and the
ATProto OAuth specification at https://atproto.com/specs/oauth.

No implementation is included here.  The ATProto OAuth flow is more complex
than standard OAuth 2.0 and has several security-critical steps.  Each
section below identifies what is required and which RFCs or specifications
govern it.  Nothing is invented; where the specification is unclear or the
correct approach is uncertain, this is stated explicitly.


Overview of the ATProto OAuth flow
-----------------------------------

ATProto uses OAuth 2.1 with the following mandatory extensions:

  - PKCE (RFC 7636) with S256 code challenge
  - Pushed Authorization Requests, PAR (RFC 9126) – the AS must receive the
    full authorization request before redirecting the user
  - Demonstrated Proof of Possession, DPoP (RFC 9449) – every token request
    and resource request must include a proof-of-possession JWT signed with
    an ephemeral key
  - Client ID Metadata Document – the client is identified by a URL that
    resolves to a JSON metadata document describing the client (there is a
    draft IETF document for this; see references below)

The identity of the resource server for a given user is determined by
resolving the user's DID document and reading the PDS endpoint from the
service entry.  The authorization server is then discovered from the PDS by
fetching the protected resource metadata (RFC 9728), which points to the
issuer.  Authorization server metadata is then fetched from the issuer
following RFC 8414.


Required packages and their roles
-----------------------------------

The following Haskell packages would be required:

  - jose (>= 0.11): JWK key generation and storage, JWT signing and
    verification.  This covers JWK thumbprints (RFC 7638), JWS signing
    (RFC 7515), and the cryptographic core of DPoP proofs.

  - crypton: SHA-256 (for PKCE code_challenge, DPoP jkt, and JWK
    thumbprints), ECDSA key generation (for the ephemeral DPoP key and
    client keys).  Already in the dependency tree via atproto-haskell-crypto.

  - http-client-tls: All HTTP communication.  Already in the tree.

  - aeson (>= 2.0): JSON parsing of server metadata, token responses, and
    client metadata documents.  Already in the tree.

  - memory or base64: URL-safe base64 encoding for PKCE verifiers and
    code challenges.  jose already brings base64-bytestring.

No Template Haskell is needed.  All JWK handling via jose is done through
its FromJSON/ToJSON instances and the Algorithms typeclass.


Data types to define
---------------------

OAuthAuthorizationServerMetadata
  Fields from RFC 8414 section 2, augmented with:
    - dpop_signing_alg_values_supported (RFC 9449 section 5.1)
    - pushed_authorization_request_endpoint (RFC 9126)
    - require_pushed_authorization_requests (RFC 9126)
    - client_id_metadata_document_supported (draft)
  Required fields: issuer, authorization_endpoint, token_endpoint.
  All other fields are optional.

OAuthProtectedResourceMetadata
  Defined in RFC 9728.  Key field: authorization_servers (list of issuer URLs).

OAuthClientMetadata
  Defined by the ATProto client ID metadata document draft.  Includes
  client_id (the URL of this document), redirect_uris, scope, grant_types,
  response_types, token_endpoint_auth_method, dpop_bound_access_tokens,
  and application_type.

TokenSet
  The in-memory representation of a token response:
    - iss, sub (DID), aud (PDS URL), scope
    - access_token (always DPoP-bound)
    - token_type (always "DPoP")
    - refresh_token (optional)
    - expires_at (derived from expires_in)

DpopKey
  A single-use or per-server ephemeral keypair for DPoP proofs.  Must be
  ES256 (P-256) or ES256K (secp256k1) depending on what the server
  advertises in dpop_signing_alg_values_supported.

StateData
  Opaque data stored between the authorization redirect and the callback:
    - state (random string)
    - nonce (optional)
    - code_verifier (the PKCE verifier; the challenge is derived from it)
    - dpopKey (the DPoP keypair used for the token exchange)
    - iss (the expected issuer)


Critical security steps
------------------------

The following steps are security-critical and must not be omitted or
approximated.  They are derived directly from the ATProto spec and the
reference implementation.

1. Issuer verification after token exchange.

   When a token response is received, the "sub" claim (a DID) must be
   resolved again (without using any cache) and the resolved PDS's
   authorization server issuer must match the issuer that issued the
   token.  This prevents an attacker-controlled server from issuing
   tokens with a victim's DID as the subject.

   This is documented in oauth-server-agent.ts as "VERY IMPORTANT" and
   is also specified in the ATProto OAuth spec.

2. DPoP proof construction (RFC 9449).

   Every request to the token endpoint and every resource request must
   include a DPoP proof header.  The proof is a signed JWT containing:
     - "jti": a fresh random identifier (prevents replay)
     - "htm": the HTTP method
     - "htu": the URL (no query string, no fragment)
     - "iat": current time
     - "ath": base64url(SHA-256(access_token)) when used at a resource server

   The DPoP key used must be consistent within a session.  The reference
   implementation uses one DPoP key per server per session.

3. PKCE (RFC 7636, S256 method only).

   The code_verifier is a cryptographically random 32-byte value encoded
   as base64url.  The code_challenge is base64url(SHA-256(code_verifier)).
   The verifier must be stored and sent with the token exchange.

4. PAR (RFC 9126).

   The full authorization request parameters must be sent to the
   pushed_authorization_request_endpoint first.  The response contains
   a request_uri that is then used as the sole parameter in the
   authorization redirect URL.  The direct authorization URL approach
   (sending all parameters in the redirect) is not used by ATProto.

5. DPoP nonce handling.

   Some servers require a server-supplied nonce in DPoP proofs.  When the
   server returns a use_dpop_nonce error or a DPoP-Nonce response header,
   the client must retry the request with the nonce included.  The nonce
   must be stored per-issuer.

6. Token binding.

   ATProto access tokens are always DPoP-bound.  The access token must
   only be used in requests that include a valid DPoP proof signed with
   the same key whose public key thumbprint (jkt) was bound at token
   issuance.


Package structure
------------------

A single new package, atproto-haskell-oauth, is proposed:

  ATProto.OAuth.Types
    All data types: OAuthAuthorizationServerMetadata,
    OAuthClientMetadata, TokenSet, StateData, etc.
    FromJSON and ToJSON instances for all server-facing types.

  ATProto.OAuth.PKCE
    generateCodeVerifier :: IO ByteString
    codeChallenge :: ByteString -> ByteString  (SHA-256 then base64url)

  ATProto.OAuth.DPoP
    generateDpopKey :: IO (JWK, PublicJWK)
    createDpopProof :: DpopProofParams -> IO SignedJWT
    DpopProofParams contains method, htu, iat, jti, ath (optional), nonce

  ATProto.OAuth.ServerMetadata
    resolveProtectedResource :: Manager -> Text -> IO (Either OAuthError OAuthProtectedResourceMetadata)
    resolveAuthorizationServer :: Manager -> Text -> IO (Either OAuthError OAuthAuthorizationServerMetadata)

  ATProto.OAuth.Client
    OAuthClient -- holds client metadata, keyset, store references
    authorize :: OAuthClient -> Text -> AuthorizeOptions -> IO AuthorizeUrl
    callback  :: OAuthClient -> CallbackParams -> IO OAuthSession
    OAuthSession -- wraps TokenSet and provides authenticated request execution

  ATProto.OAuth
    Top-level re-export with usage examples.


Dependencies
-------------

  jose          >= 0.11 && < 0.12  (JWK, JWS, JWT; no TH required)
  crypton       (already present)
  http-client   (already present)
  http-client-tls (already present)
  aeson         (already present)
  text          (already present)
  bytestring    (already present)
  memory        (for constant-time ByteString comparison)
  time          (for iat claim)

The jose library provides type-safe JWK generation, JWS signing, and
algorithm negotiation.  It uses crypton as its cryptographic backend.
No Template Haskell is used in our code; the jose library itself does
not require TH for basic usage.


What is not clear and must not be assumed
------------------------------------------

The following aspects of the ATProto OAuth flow require careful reading
of the evolving specification before implementation:

  - The exact format and validation rules for the client_id URL and the
    client metadata document are governed by a draft that has not yet
    been finalised (draft-ietf-oauth-client-id-metadata-document).

  - The scope negotiation rules specific to ATProto (which scopes are
    required, which are optional, and how they map to access levels) are
    described in the ATProto spec but may change.

  - The refresh token rotation behaviour (whether the server always
    rotates refresh tokens and how to handle the case where the old
    token becomes invalid before the new one is stored) requires careful
    design of the session store interface.

  - DPoP key rotation policy (how frequently to rotate ephemeral keys,
    and whether different endpoints within the same session must use the
    same key) is specified in RFC 9449 but the ATProto profile may
    impose additional constraints.


References
-----------

  RFC 9449 – OAuth 2.0 Demonstrating Proof of Possession (DPoP)
    https://www.rfc-editor.org/rfc/rfc9449
  RFC 9126 – OAuth 2.0 Pushed Authorization Requests
    https://www.rfc-editor.org/rfc/rfc9126
  RFC 8414 – OAuth 2.0 Authorization Server Metadata
    https://www.rfc-editor.org/rfc/rfc8414
  RFC 9728 – OAuth 2.0 Protected Resource Metadata
    https://www.rfc-editor.org/rfc/rfc9728
  RFC 7638 – JSON Web Key (JWK) Thumbprint
    https://www.rfc-editor.org/rfc/rfc7638
  draft-ietf-oauth-v2-1 – OAuth 2.1
    https://datatracker.ietf.org/doc/draft-ietf-oauth-v2-1/
  draft-ietf-oauth-client-id-metadata-document
    https://www.ietf.org/archive/id/draft-ietf-oauth-client-id-metadata-document-00.html
  ATProto OAuth specification
    https://atproto.com/specs/oauth
