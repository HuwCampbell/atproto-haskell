API Alignment Audit
===================

Last updated: 2026-04-30.

This document describes the current state of each package in
atproto-haskell and how closely its public API aligns with the
corresponding reference TypeScript package maintained by Bluesky.


atproto-haskell-syntax  (@atproto/syntax)
------------------------------------------

The Haskell package covers every identifier type in the reference
implementation: DID, handle, NSID, AT-URI, TID, and record key.
Validation rules follow the specification precisely, including handle
label length limits, TLD restrictions, NSID depth limits, AT-URI
component parsing, and TID base32 encoding.  The top-level
ATProto.Syntax module re-exports all sub-modules; each sub-module
uses explicit export lists.

The Haskell API diverges from the TypeScript in that validation
functions return Either String rather than throwing exceptions.  This
is idiomatic Haskell 2010 and makes error handling explicit at the
call site.  The reference package exposes ensure* helpers that throw;
these are omitted in favour of the Either-based interface, which
subsumes them.

Normalisation (lower-casing) is available via parseAndNormaliseHandle
and normaliseHandle.  The isDisallowedTld predicate matches the policy
list in the reference package.  Test coverage uses Hedgehog property
tests and matches the known-good/known-bad value tables in the
reference test suite.


atproto-haskell-crypto  (@atproto/crypto)
------------------------------------------

The Haskell package supports P-256 and secp256k1, matching the two
curves used by the AT Protocol.  Key generation uses the OS CSPRNG
via the crypton library.  Signing produces 64-byte compact (r || s)
signatures with low-S normalisation enforced, which is required by
the specification.  Verification supports both Strict mode (low-S
only) and Permissive mode (any valid S), mirroring verifySig and
verifyDidSig in the reference.

The did:key encoding and decoding (pubKeyToDidKey / didKeyToPubKey)
uses the correct multicodec varint prefixes: 0x1200 for P-256 and
0xe701 for secp256k1.  Base58btc encoding is implemented in pure
Haskell with no additional dependencies.  The Multikey module provides
encodeMultikey and decodeMultikey for multibase-encoded public keys
used in DID documents.

The reference package also exports utility functions for importing raw
private keys and for parsing compact signatures.  importPrivKey is
present; compact signature parsing is handled implicitly within
verify.  The reference TypeScript package's ExportableKeypair interface
exposes an export() method that returns the raw private key bytes; the
Haskell package does not expose a corresponding exportPrivKey function,
though the private key bytes are accessible through the PrivKey type's
internal field.

Test coverage includes round-trip did:key tests and signing/
verification tests for both curves.  The reference package includes
test vectors from the specification; the Haskell suite does not yet
import those exact vectors.


atproto-haskell-did  (no single equivalent; @atproto/identity partially)
--------------------------------------------------------------------------

This package provides DID document parsing and resolution.  The
DidDocument, VerificationMethod, and Service types correspond
directly to the DID Core specification fields parsed by the reference
resolver.  FromJSON instances are hand-written using aeson 2.x without
Template Haskell.

Four resolvers are implemented:

  - PlcResolver: did:plc via the PLC directory at plc.directory.
  - WebResolver: did:web via the .well-known/did.json endpoint.
  - DispatchResolver: auto-dispatches to PlcResolver or WebResolver
    based on the DID method prefix, created via newDispatchResolver
    or defaultDispatchResolver.
  - CachingResolver: a transparent caching wrapper around any
    DidResolver with a configurable TTL (default 1 hour).
    newCachingResolver accepts a custom TimeSpec;
    defaultCachingResolver uses 3600 seconds.
    refreshResolve bypasses the cache for force-refresh scenarios.

defaultDidResolver chains all four layers (PLC + Web -> Dispatch ->
Caching) into a single ready-to-use resolver.

Both PlcResolver and WebResolver match the reference behaviour,
including the AT Protocol restriction that did:web identifiers
containing path components are rejected.

The reference identity package combines DID and handle resolution in
a single IdResolver class.  The Haskell design keeps them in separate
packages (atproto-haskell-did for DID documents, atproto-haskell-identity
for handle resolution), which is a deliberate decomposition into smaller
units of responsibility.


atproto-haskell-identity  (@atproto/identity, handle portion)
--------------------------------------------------------------

The HandleResolver type mirrors the TypeScript HandleResolver class.
The two resolution methods are implemented: DNS TXT lookup on
_atproto.<handle> and HTTP GET to https://<handle>/.well-known/atproto-did.
The fallback strategy (DNS first, then HTTP, then backup nameservers)
matches the reference.  The pure helper parseDnsResult is a direct port
of the TypeScript function of the same name.

The reference resolves backup nameserver hostnames to IP addresses at
runtime.  The Haskell implementation requires IP addresses directly,
since the dns library's RCHostNames constructor only accepts numeric
addresses.  This is documented in HandleResolverOpts.

The reference HandleResolver accepts a timeout option.  The Haskell
implementation uses the dns library's default timeout; configurable
timeouts are not yet exposed.

The reference identity package also includes an IdResolver that
combines handle resolution with DID resolution and performs
cross-validation (checking that the DID document's alsoKnownAs field
contains the original handle).  This cross-validation step is not yet
implemented in Haskell.


atproto-haskell-ipld  (IPLD value types)
------------------------------------------

The package provides the intermediate representation for AT Protocol
record data, bridging between DAG-JSON (Aeson) and DAG-CBOR (cborg)
serialisation formats.

The central type is LexValue, with constructors for null, bool, int
(Int64, no floats per spec), string, bytes, CID link, blob reference,
array, and object.  The Cid newtype wraps a Text-encoded CID string.
BlobRef carries a CID, MIME type, and size.

DAG-JSON encoding and decoding (ATProto.Ipld.Json) handles the special
$link, $bytes, and $type conventions.  DAG-CBOR encoding and decoding
(ATProto.Ipld.Cbor) handles CBOR tag-42 for CID links and produces
canonical shortest-form output.


atproto-haskell-lexicon  (@atproto/lexicon -- types)
-----------------------------------------------------

The Haskell package provides data types for the full Lexicon schema
language: all primitive types, IPLD types, references, blobs, arrays,
objects, XRPC parameter/body/error types, query/procedure/subscription
definitions, record types, and the top-level LexiconDoc.  These match
the discriminated-union structure of the TypeScript implementation.

FromJSON instances cover all types, enabling eitherDecode on any
.lexicon.json file.  The com.atproto.repo.listRecords lexicon parses
correctly in the test suite.

The reference @atproto/lexicon package also provides:
  - schema validation (checking that a record value conforms to a schema)
  - ToJSON instances for round-tripping
  - structural merging of lexicon documents

ToJSON instances and structural merging are not implemented in
atproto-haskell-lexicon.  Schema validation is provided by the
companion package atproto-haskell-lex (see below).


atproto-haskell-lex  (@atproto/lexicon -- codecs and validation)
-----------------------------------------------------------------

This package provides profunctor-style codec combinators for the
AT Protocol Lexicon type system, bridging typed Haskell values to JSON
and CBOR serialisation.

A Codec carries three things: a structural LexSchema, a decoder, and
a writer.  The codec DSL covers all AT Protocol primitives (null, bool,
int, text, string, bytes, cid, blob, datetime, atUri, did, handle,
uri), composites (array, nullable, fallback), structured records via
the StructBuilder/StructCodec pattern (record, requiredField,
optionalField, fallbackField), and tagged unions (union through
union5).

ATProto.Lex.Json and ATProto.Lex.Cbor provide bridge modules for
encoding and decoding through Aeson and cborg respectively.

ATProto.Lex.Validate implements structural schema validation:
validate :: LexiconDoc -> Text -> LexSchema -> [ValidationError]
checks that a codec's embedded schema agrees with a parsed lexicon
document.  Validation errors include FieldMissing, FieldExtra,
TypeMismatch, RequiredMismatch, and DefinitionNotFound.

This validation checks schema structure (codec against lexicon
specification), which is the compile-time guarantee that a codec
correctly describes a lexicon.  The reference @atproto/lexicon
package's runtime validation (assertValidRecord, assertValidXrpcParams,
etc.) checks arbitrary JSON values against a schema at runtime; this
runtime value-level validation is not yet implemented.


atproto-haskell-xrpc  (@atproto/xrpc)
---------------------------------------

The XrpcClient typeclass and the XrpcRequest/XrpcResponse/XrpcError
types correspond closely to the TypeScript client interface.  The
xrpcQuery and xrpcProcedure helpers match the query/procedure
distinction in the reference.

The reference package includes typed helpers for decoding procedure
inputs and query outputs against a Lexicon schema.  The Haskell
package returns raw lazy ByteString bodies and leaves JSON decoding
to the caller.  This is simpler but means schema-validated request
construction is deferred to higher-level packages.

Error handling maps HTTP error bodies with the {error, message}
structure to XrpcError values, matching the reference.


atproto-haskell-xrpc-http  (runtime backend; no direct TS equivalent)
-----------------------------------------------------------------------

HttpXrpcClient provides the XrpcClient instance backed by
http-client-tls.  URL construction (/xrpc/<nsid>), query-string
encoding, POST body forwarding, and header forwarding all match the
behaviour of the reference Node.js fetch-based client.  A shared
Manager supports connection pooling.

Authentication tokens are passed via xrpcReqHeaders; the client
forwards all headers unchanged.  This matches the reference approach of
accepting headers as a parameter rather than storing them in the client.


atproto-haskell-xrpc-server  (@atproto/xrpc-server)
----------------------------------------------------

The package provides a WAI Middleware abstraction for serving XRPC
endpoints, the server-side counterpart to atproto-haskell-xrpc and
atproto-haskell-xrpc-http.

Handlers are parametric in the application monad m (constrained to
MonadIO) and in the caller identity type did.  This lets users write
handlers in ReaderT AppEnv IO or any other MonadIO stack and supply a
runner once at the WAI boundary, rather than threading environment
values through every handler.  The pattern is analogous to hoistServer
in Servant.

The XrpcServer routing table is keyed by (XrpcMethod, NSID) and built
with makeServer from a list of XrpcEndpoint values registered via the
query and procedure smart constructors.

xrpcMiddleware intercepts /xrpc/<nsid> paths, parses and validates the
NSID, dispatches to the matching handler, and serialises results.
Unknown paths fall through to the next WAI Application.  xrpcApplication
wraps the middleware into a standalone Application that returns 404 for
non-XRPC paths.

HTTP routing rules match the reference:
  - Invalid NSID in path          -> 400 InvalidNSID
  - Valid NSID, wrong HTTP method  -> 405 MethodNotAllowed
  - Valid NSID, no handler         -> 501 MethodNotImplemented
  - Successful handler result      -> 200 with JSON body
  - XrpcAccepted                   -> 202, no body
  - XrpcBadRequest                 -> 400 with {error, message} body
  - XrpcUnauthorised               -> 401 with {error, message} body

Error responses are serialised as {"error":"...","message":"..."}
with Content-Type: application/json without an aeson dependency.

An AuthVerifier hook is provided for authentication.  Attach any
MonadIO-compatible verifier with withAuthVerifier; the middleware runs
it before every handler dispatch and returns HTTP 401 on AuthFailed.
On success the caller's DID is available as xsrCaller in the request.
Built-in auth schemes supported out of the box (by composing with other
packages):
  - Service-to-service JWTs via ATProto.ServiceAuth.verifyServiceJwt
  - OAuth DPoP bearer tokens via ATProto.OAuth.Provider.Verifier
  - Any custom Basic / API-key scheme


atproto-haskell-repo  (portions of @atproto/api)
--------------------------------------------------

The package provides typed XRPC bindings organised by namespace,
covering the core com.atproto.* methods and select app.bsky.* types.
Each binding module follows a consistent pattern: request/params types,
response types, codec definitions (using atproto-haskell-lex), and a
client function taking any XrpcClient backend.

com.atproto.repo.* bindings (11 modules):

  - ListRecords: ListRecordsParams covers all five query parameters
    (repo, collection, limit, cursor, reverse).
  - PutRecord: PutRecordRequest covers repo, collection, rkey, record,
    and optional validate flag; PutRecordResponse returns URI, CID, and
    CommitMeta.
  - DeleteRecord: DeleteRecordRequest and DeleteRecordResponse.
  - CreateRecord: CreateRecordRequest (repo, collection, optional rkey,
    record, optional validate) and CreateRecordResponse (URI, CID,
    commit).
  - GetRecord: GetRecordParams (repo, collection, rkey, optional CID)
    and GetRecordResponse (URI, CID, value).
  - DescribeRepo: DescribeRepoParams and DescribeRepoResponse (handle,
    DID, DID document, collections, handleIsCorrect).
  - ApplyWrites: atomic batch write operations.
  - CommitMeta: shared commit metadata type.
  - UploadBlob / GetBlob: blob upload and retrieval.
  - GetProfile: app.bsky.actor.getProfile wrapper.
  - ListMissingBlobs: list blobs referenced but not stored.

com.atproto.server.* bindings (8 modules):

  - CreateAccount, CreateSession, RefreshSession, DeleteSession,
    GetSession, DescribeServer, GetServiceAuth, CheckAccountStatus.
  - Defs: shared server definition types.

com.atproto.identity.* bindings (5 modules):

  - ResolveHandle, ResolveDid, ResolveIdentity, UpdateHandle.
  - Defs: shared identity definition types.

com.atproto.sync.* bindings (8 modules):

  - GetBlob, GetLatestCommit, GetRepo, GetRepoStatus, ListBlobs,
    ListRepos, NotifyOfUpdate, RequestCrawl.

com.atproto.label.* (1 module):

  - Defs: label definition types.

app.bsky.* (2 modules):

  - Feed.Post: post record type.
  - Feed.GetFeedSkeleton: feed generator skeleton.


atproto-haskell-car  (@atproto/repo -- CAR)
--------------------------------------------

CAR v1 (Content Addressable aRchive) reading and writing are fully
implemented.

Reading: the parser covers the varint-length-prefixed header (DAG-CBOR
map with version and roots fields), CBOR tag-42 CID encoding/decoding,
and the block sequence.  Binary CIDv1 parsing dynamically determines
CID length by reading version, codec, and multihash header varints.
Display uses multibase base32lower (prefix 'b'), compatible with the
existing string-based ATProto.Ipld.Value.Cid type.  readCar handles
zero or more roots; readCarWithRoot enforces exactly one root.

Writing: writeCar encodes a list of root CIDs and a BlockMap into a
valid CAR v1 byte string.  writeCarWithRoot is the single-root
convenience wrapper.  Blocks are written in ascending CID order with
varint-encoded length prefixes.

The BlockMap type (Map CidBytes ByteString) is the central data
structure shared between CAR, MST, and PDS packages.

Shared DAG-CBOR helpers in ATProto.Car.DagCbor (encodeCidTag42,
decodeCidTag42, encodeNullableCidTag42, decodeNullableCidTag42,
skipValue) are used by atproto-mst, atproto-pds, and
atproto-repo-verify for canonical CBOR encoding.  cidForDagCbor
computes the CIDv1 (SHA-256, DAG-CBOR codec) for a byte string.

Gap: no CAR v2 support (the reference repo package supports both).


atproto-haskell-mst  (@atproto/repo -- MST)
-------------------------------------------

Merkle Search Tree operations are fully implemented, including both
reading and writing:

  - Node decoding: NodeData and TreeEntry from DAG-CBOR, including
    nullable tag-42 CID fields for left subtrees and right subtrees.
  - Node encoding: encodeNode serialises an MST node to canonical
    DAG-CBOR for CID computation and block storage.
  - Layer computation: leadingZerosOnHash counts leading 2-bit zero
    pairs in the SHA-256 digest, matching the reference exactly.
    Test vectors from the TypeScript suite are verified.
  - MST construction: singleton creates a single-entry tree;
    fromList and fromNonEmpty batch-construct from sorted key-value
    pairs; insert adds or updates a single key in an existing tree,
    handling layer-based spine node creation and subtree splitting.
  - Point lookup: lookup and member for key queries; toList for full
    leaf enumeration; rootCid for the tree root CID.
  - Serialisation: toBlockMap serialises the entire tree to a BlockMap
    suitable for CAR encoding and block storage.
  - Tree diff: diff computes WCreate/WUpdate/WDelete descriptors
    between two trees.  Equal subtree CIDs are short-circuited.
  - Proof verification: verifyProofs checks each RecordOp against the
    live MST, comparing found CIDs with claimed CIDs.

The write path (insert, fromList, toBlockMap) corresponds to the Repo
class in the TypeScript @atproto/repo package, completing the
read/write coverage needed by the PDS.


atproto-haskell-repo-verify  (@atproto/repo -- commit verification)
-------------------------------------------------------------------

Full commit verification pipeline:

  - decodeCommit: DAG-CBOR map decoding for the Commit type, handling
    optional prev field (nullable tag-42 CID) and all required fields.
  - resolveAtprotoKey: extracts the #atproto VerificationMethod from
    a DidDocument and decodes the multibase public key via
    ATProto.Crypto.Multikey.decodeMultikey.
  - verifyCommitSig: re-encodes the unsigned commit in canonical
    DAG-CBOR field order (did, rev, data, prev, version) and calls
    ATProto.Crypto.EC.verify Strict.
  - verifyCommitCar: full pipeline -- parse CAR, decode commit, assert
    DID, extract key, verify sig, verify MST proofs, return diff.

Gap: encodeUnsignedCommit always includes the prev field (as CBOR
null when absent).  Legacy v2 commits that omit prev entirely will
produce a signature mismatch.  V3 commits (current standard) always
include prev and are handled correctly.

Gap: no caching of resolved DID documents.  The authenticated
firehose client calls afcResolveDid twice on key-rotation failures;
callers must implement caching to avoid excessive network requests.


atproto-haskell-firehose  (@atproto/sync -- Firehose)
------------------------------------------------------

WebSocket firehose client with event decoding and optional commit
verification:

  - Event ADTs: CommitEvent, IdentityEvent, AccountEvent, SyncEvent,
    InfoEvent, FirehoseEvent with FEUnknown for forward compatibility.
  - RepoOp with OpAction (create/update/delete) and nullable text CID.
  - decodeFrame: DAG-CBOR frame decoder dispatching on the t field in
    the header map.  CID fields are decoded from tag-42 bytes and
    re-encoded as multibase text.
  - runFirehose: WSS client via wuss, cursor tracking in IORef,
    auto-reconnect with last-seen sequence number.
  - runAuthFirehose: per-commit verification using verifyCommitCar,
    tooBig guard, key-rotation retry (two DID resolves on VerifyBadSig).

Gap: toRecordOp sets ropCid = Nothing for all ops.  For creates and
updates the text CID from RepoOp.ropCid should be decoded to CidBytes
and compared against the MST.  Currently verifyProofs only confirms
presence/absence, not the exact record CID.  Fixing this requires a
textCidToBinary helper (multibase base32lower decode).

Gap: no cursor persistence.  Reconnection uses the in-memory last-seen
sequence number.  After a process restart the client resumes from the
configured initial cursor (or the beginning of the firehose).

Gap: no connection backoff/retry delay.  Rapid reconnection on error
may be rate-limited by the relay.

Gap: #identity events are not cross-validated against the DID
document's alsoKnownAs field (requires bidirectional handle
verification not yet present in atproto-haskell-identity).


atproto-haskell-service-auth  (service-to-service authentication)
------------------------------------------------------------------

Service authentication allows one ATProto service to make authenticated
calls to another by presenting a short-lived signed JWT.  The Haskell
package covers both sides of that exchange.

On the creation side, createServiceJwt accepts a ServiceJwtParams record
containing the issuer DID, audience DID, optional lexicon method binding
(lxm), private key, and expiry duration; it returns a compact JWS token
signed with either ES256 (P-256) or ES256K (secp256k1) depending on the
key type.  A random JTI is generated for each token.  The convenience
helper createServiceAuthHeaders returns a ready-to-use Authorization
header value.

On the verification side, verifyServiceJwt decodes the JWT header and
payload, checks the expiry and audience, optionally validates the lxm
claim, and verifies the signature using a caller-supplied key-lookup
callback.  The callback is first invoked with a cache hint of True; if
signature verification fails the library retries with the hint set to
False (force-refresh), mirroring the key-rotation handling in the
reference auth-verifier.ts.  The error type ServiceAuthError covers all
rejection reasons (malformed JWT, expired, wrong audience, wrong lexicon
method, bad signature, invalid issuer).

The reference implementation lives in packages/pds/src/auth-verifier.ts
and the shared packages/common library.  The Haskell coverage is
functionally complete for the service-auth subsystem.


atproto-haskell-oauth  (@atproto/oauth-client)
-----------------------------------------------

The package implements the ATProto OAuth 2.1 profile on the client side,
covering the full authorisation flow from server-metadata discovery
through token refresh.

Server metadata is discovered via fetchProtectedResourceMetadata (RFC
9728) and fetchAuthorizationServerMetadata (RFC 8414), with
getIssuerForPds as a convenience helper that chains both calls.  The
resulting OAuthAuthorizationServerMetadata exposes all fields required
for PAR (RFC 9126) and DPoP (RFC 9449) flows.

PKCE (RFC 7636) uses the S256 method only, matching the ATProto
requirement.  generateCodeVerifier produces a 32-byte CSPRNG value
encoded in base64url; codeChallenge computes the SHA-256 digest of the
verifier.

DPoP proof tokens are built by createDpopProof, which takes a
DpopClaims record (htm, htu, ath, optional nonce) and an ephemeral P-256
DpopKey.  Public JWK export via dpopPublicJwk is provided for the PAR
request.

The OAuthClient type is the central state machine.  authorize initiates
the flow by sending a PAR request, storing a StateData value, and
returning the redirect URL.  callback completes the flow, exchanges the
authorisation code for tokens, and stores a Session containing the
DPoP key and TokenSet.  getTokenInfo checks the current token and
performs a DPoP-bound refresh if needed, caching per-server nonces and
retrying once on use_dpop_nonce errors.  deleteSession removes a stored
session.

State and session storage are abstracted behind the StateStore and
SessionStore interfaces.  In-memory implementations are provided via
newInMemoryStateStore and newInMemorySessionStore; callers requiring
persistence implement the interface themselves.

OAuthXrpcClient wraps an OAuthClient and a Session to provide a full
XrpcClient instance with automatic DPoP proof generation and transparent
token refresh on every request.

The reference packages covered are @atproto/oauth-client,
@atproto/oauth-client-node, @atproto/oauth-types, @atproto/jwk, and
@atproto/oauth-scopes.


atproto-haskell-oauth-provider  (@atproto/oauth-provider -- server side)
------------------------------------------------------------------------

The package implements server-side OAuth 2.1 token verification and
DPoP proof validation, covering the core security operations that a
PDS or resource server needs to authenticate incoming requests.

DPoP nonce management (ATProto.OAuth.Provider.DPoP.Nonce) implements
HMAC-based rotating nonces.  newNonceState initialises mutable state
with a secret and rotation interval; nextNonce produces the current
valid nonce; checkNonce validates a presented nonce against the
current and previous rotation windows.

DPoP proof verification (ATProto.OAuth.Provider.DPoP.Verifier)
implements RFC 9449 server-side validation: JWS signature verification,
EC key extraction, htm/htu binding checks, nonce validation, ath
(access token hash) claim validation, and iat recency checking.

Access token management (ATProto.OAuth.Provider.Token) implements
RFC 9068 at+jwt tokens: createAccessToken issues a signed JWT with
the standard claims (iss, sub, aud, jti, scope, client_id, iat, exp,
cnf.jkt); verifyAccessToken decodes and validates tokens.  Signing
keys are EC P-256.

Request authentication (ATProto.OAuth.Provider.Verifier) ties the
above together: authenticateRequest parses the Authorization header,
verifies the DPoP proof, verifies the access token, and checks the
DPoP binding (cnf.jkt matches the proof key).

The reference @atproto/oauth-provider package is considerably larger,
encompassing the full authorisation server with consent UI, client
registration, authorisation code grants, and session management.  The
Haskell package covers only the token-verification and DPoP subsystems
needed to protect resource-server endpoints.

Gap: authorisation code grant flow (issuing codes, exchanging them for
tokens) is not implemented.  A PDS acting as a full authorisation
server would need to add these flows.

Gap: client registration and metadata validation are not implemented.


atproto-haskell-pds  (AT Protocol Personal Data Server core)
--------------------------------------------------------------

The package provides the core library for building a Personal Data
Server, including type-class-based storage abstractions, repository
management, commit signing, account management, and blob storage.

Storage is abstracted behind three type classes:

  - BlockStore: content-addressed block storage (CID -> bytes), with
    getBlock, putBlock, and deleteBlock.
  - RepoStore: repository head pointer (DID-scoped, single head CID),
    with getRepoHead and setRepoHead.
  - BlobStore: streaming blob storage with a temp-stage-then-commit
    lifecycle (putTemp, makePermanent, streamBytes, delete).

Account management is abstracted behind the AccountStore type class
(ATProto.PDS.AccountStore):

  - createAccount / getAccount / updateAccount / deleteAccount: CRUD
    for Account records (DID, handle, email, deactivated flag).
  - storePassword / getPasswordHash: bcrypt password storage.
  - getSigningKey: retrieve the actor's commit-signing key.
  - storePlcRotationKey / getPlcRotationKey: optional PLC rotation key
    for did:plc-based accounts.
  - getJwtKey: retrieve the 32-byte HMAC-SHA256 key used to sign JWTs.
  - storeRefreshToken / getRefreshToken / revokeRefreshToken /
    revokeRefreshTokensByDid: refresh-token lifecycle management.

Session token helpers (createAccessToken, createRefreshToken,
verifyAccessToken, verifyRefreshToken) issue and verify compact HS256
JWTs following the upstream PDS format: access tokens use typ "at+jwt"
(120-minute TTL) and refresh tokens use typ "refresh+jwt" (90-day TTL
with server-side jti tracking for revocation).

The actor-store abstraction (ATProto.PDS.ActorStore) bundles a DID
with its per-actor storage value.  ActorStoreBackend is a factory that
opens and closes scoped stores; withActorStore provides bracket-style
resource safety.  This mirrors the actor-store subsystem in the
reference PDS.

Concrete backends provided:

  - InMemoryBackend / InMemoryActorStore: IORef-wrapped Maps, suitable
    for tests and short-lived processes.
  - FileBackend / FileActorStore: filesystem backend storing blocks as
    individual files named by base32-encoded CID; blob staging via a
    temp directory with atomic rename; implements BlockStore, RepoStore,
    and BlobStore.
  - InMemoryAccountStore: IORef-wrapped Maps for all account data;
    generates a fresh JWT key at construction.
  - FileAccountStore: persists accounts as key=value text files under
    basedir/accounts/<did>/, password hashes, signing keys, and PLC
    rotation keys; persists the JWT key to basedir/jwt.key so session
    tokens survive process restarts; refresh tokens stored as individual
    files under basedir/refresh_tokens/<jti>.

Commit signing (ATProto.PDS.Commit) produces v3 signed commits in
canonical DAG-CBOR field order (did, rev, sig, data, prev, version).
createSignedCommit accepts a DID, private key, MST root CID, previous
commit CID, and revision TID, returning the signed commit bytes and
CID.

Repository operations (ATProto.PDS.Repo) build on BlockStore,
RepoStore, MST, and commit signing:

  - initRepo: creates an empty repository with a signed initial commit.
  - createRecord: inserts a new record, produces a new commit.
  - getRecord: retrieves a record by collection and record key.
  - listRecords: lists all records in a collection.
  - deleteRecord: removes a record, produces a new commit.
  - applyWrites: atomic batch of Create/Update/Delete operations in
    a single commit using zipperDiff for efficient block diffing.
  - exportRepoCar: exports the full repository state as CAR v1 bytes.
  - importRepoCar: imports CAR v1 bytes into block storage and sets
    the repository head.

The PdsError type covers all failure modes (repo not found, already
exists, block not found, MST error, commit decode error, record
exists, record not found).

Gap: no sequencer (outbound firehose producer).  Commits are stored
but not broadcast to WebSocket subscribers.

Gap: no PreferenceStore type class.  User preference storage is not
yet abstracted.


atproto-haskell-tap  (Trusted Application Pass -- @atproto/tap)
---------------------------------------------------------------

TAP is the notification push mechanism used by ATProto applications to
receive real-time record and identity events from an aggregator service.
The Haskell package covers all three access patterns: HTTP admin API,
WebSocket subscription, and webhook receiver.

The event ADT has two constructors: TapRecordEvent wrapping RecordEvent
(DID, collection, rkey, action, CID, optional record value, revision
TID, live flag) and TapIdentityEvent wrapping IdentityEvent (DID,
handle, active flag, optional status).  TapUnknown preserves unknown
event payloads for forward compatibility.  parseTapEvent parses the JSON
envelope.

The HTTP admin client (TapClient) wraps a TLS manager and base URL.
addRepos and removeRepos manage the set of DIDs tracked by the TAP
instance; resolveDid proxies DID resolution; healthCheck performs a
liveness probe.  Optional Basic auth for the admin password is applied
when TapConfig includes it.

The WebSocket client is started with runTapWebSocket.  It connects over
plain or TLS WebSocket, calls the caller-supplied onEvent callback for
each parsed TapEvent, reconnects automatically with exponential backoff
(1 s to 30 s), and optionally sends per-event acknowledgements when
twcEnableAcks is True.

The webhook module provides assureAdminAuth, which verifies a Basic auth
header against an expected admin password; this is used when a TAP
instance is configured to push events via HTTP POST rather than
WebSocket pull.

Gap: the WebSocket client does not persist the event offset across
process restarts.  Reconnection after a crash resumes from the beginning
of the stream rather than from the last acknowledged sequence.

Gap: the client streams all events for tracked repositories without
server-side filtering by collection or action type.


atproto-haskell-plc  (PLC directory operations)
------------------------------------------------

The package provides types and an HTTP client for creating and
updating did:plc identifiers via the PLC directory.

UnsignedPlcOp captures the full contents of a PLC genesis or
update operation: rotation keys (did:key list), verification methods
(map of label to did:key, e.g. {"atproto": "did:key:z..."}),
alsoKnownAs aliases (at:// handle URIs), services (map of service ID
to PlcService with type and endpoint), and optional prev CID for
updates.

signPlcOp signs an UnsignedPlcOp with a rotation key using the
EC signing function from atproto-haskell-crypto, producing a base64url
signature attached as a PlcOp.  encodePlcOpForSigning encodes the
unsigned operation in canonical DAG-CBOR field order (prev, type,
services, alsoKnownAs, rotationKeys, verificationMethods).

plcOpDid derives the did:plc identifier from a genesis operation: it
SHA-256 hashes the DAG-CBOR encoding, takes the first 15 bytes, and
base32-encodes them.  This matches the PLC specification exactly.

PlcClient wraps an HTTP Manager and base URL.  submitPlcOp posts a
signed PlcOp as JSON to POST /<did>.  resolvePlcDid fetches the DID
document via GET /<did>.  defaultPlcEndpoint points to the live
plc.directory.

Gap: no support for plc_tombstone operations (account deletion from
the PLC directory).

Gap: no support for verifying existing PLC operation chains (audit
log traversal and signature verification for the full history).


Personal Data Server -- Readiness Analysis
-----------------------------------------

A Personal Data Server (PDS) is the home server for an ATProto user.
It stores the user's repository (a signed Merkle Search Tree of
records), handles account creation and login, serves the
com.atproto.* XRPC namespace, manages blob storage, runs a sequencer
that emits the outbound event stream (firehose), acts as an OAuth
authorisation server, and proxies unimplemented queries to an AppView.
The reference implementation is in packages/pds/ of the
bluesky-social/atproto repository.

The table below maps each major PDS subsystem to the Haskell equivalents
and assesses readiness.

| Upstream subsystem | Haskell equivalent | Status |
|---|---|---|
| account-manager/ -- account DB, email verification, sessions | atproto-pds AccountStore typeclass with InMemory and FileSystem backends; HS256 JWT access/refresh token lifecycle | Present (basic) |
| actor-store/ -- per-actor repo storage | atproto-pds BlockStore + RepoStore + BlobStore type classes with InMemory and FileSystem backends; ActorStore + ActorStoreBackend factory | Present |
| api/ -- XRPC handlers for com.atproto.* | atproto-repo covers repo.* (createRecord, getRecord, listRecords, putRecord, deleteRecord, describeRepo, applyWrites, uploadBlob, getBlob, listMissingBlobs), server.* (createAccount, createSession, refreshSession, deleteSession, getSession, describeServer, getServiceAuth, checkAccountStatus), identity.* (resolveHandle, resolveDid, resolveIdentity, updateHandle), sync.* (getBlob, getLatestCommit, getRepo, getRepoStatus, listBlobs, listRepos, notifyOfUpdate, requestCrawl), label.defs | Present (typed bindings for all core methods) |
| auth-verifier.ts -- JWT/OAuth token verification | atproto-service-auth (service-to-service JWTs); atproto-oauth-provider (DPoP proof verification, at+jwt token verification, request authentication); atproto-xrpc-server AuthVerifier hook wires them to XRPC handlers; atproto-pds AccountStore verifyAccessToken for HS256 session tokens | Present |
| auth-routes.ts / auth-scope.ts -- OAuth provider routes | atproto-oauth-provider covers token verification and DPoP; authorisation code grant flow absent | Partial |
| sequencer/ -- outbound event sequencer (firehose emitter) | atproto-firehose covers the consumer side | Missing (producer side) |
| did-cache/ -- persisted DID document cache | CachingResolver in atproto-did | Present |
| disk-blobstore.ts / aws -- blob storage | BlobStore typeclass + FileActorStore BlobStore instance (temp-stage then atomic rename) | Present (basic) |
| mailer/ -- email for verification and password reset | None | Missing (out of scope for library) |
| handle/ -- handle resolution and validation | atproto-identity + atproto-syntax | Present |
| config/ -- server configuration | None | Missing (application concern) |
| repo/ (PDS-internal) -- repo mutation (create/update/delete) | atproto-pds (initRepo, createRecord, getRecord, listRecords, deleteRecord, applyWrites) using atproto-mst (insert, fromList, toBlockMap, zipperDiff) for efficient MST mutations and atproto-pds Commit for v3 signing | Present |
| plc/ -- did:plc genesis and updates | atproto-plc (UnsignedPlcOp, signPlcOp, plcOpDid, submitPlcOp, resolvePlcDid) | Present (basic) |
| well-known.ts -- .well-known/atproto-did and did.json serving | None | Missing (trivial, application concern) |
| crawlers.ts -- notifying relays of new commits | None | Missing |
| pipethrough.ts -- proxying unimplemented queries to AppView | atproto-xrpc-http | Possible |
| XRPC server framework | atproto-xrpc-server: WAI Middleware routing NSID paths, serialising errors, parametric handler monad | Present |

The most significant remaining gaps for a PDS implementation are as
follows.

Account and session management -- partial.  The AccountStore typeclass
and both InMemory and FileSystem backends are now implemented.  Session
tokens (HS256 access/refresh JWTs) are issued and verified.  What
remains is wiring these into live XRPC handler implementations:
com.atproto.server.createSession, refreshSession, deleteSession,
getSession must call the AccountStore methods and return the correct
JSON shapes.

Blob storage -- present.  The BlobStore typeclass is defined and the
FileActorStore provides a working file-system implementation using
temp-stage-then-atomic-rename.  The XRPC com.atproto.repo.uploadBlob
and getBlob handlers must be wired to BlobStore operations.

Sequencer (outbound firehose producer).  The atproto-firehose package
is a firehose consumer.  A PDS must emit events: on every commit it
must serialise a CommitEvent (including the CAR block), sequence it
with a monotonically increasing sequence number, and broadcast it to
connected WebSocket subscribers.  This is the sequencer/ subsystem in
the reference implementation.

OAuth authorisation code grant.  The atproto-oauth-provider package
handles token verification and DPoP but does not implement the
authorisation code grant flow (issuing codes, consent UI integration,
code-for-token exchange).  A full PDS authorisation server requires
these additional flows.  For a minimal PDS using only handle/password
sessions this can be deferred.

Application wiring.  There is no top-level atproto-pds-server (or
similar) package that wires all the library components into a runnable
Warp/WAI application.  All the library pieces exist; what is missing
is the assembly.

What is already present and useful for a PDS implementation:

The atproto-pds package provides full repository operations
(initRepo, createRecord, getRecord, listRecords, deleteRecord,
applyWrites) with pluggable storage backends and v3 commit signing.
CAR export and import round-trip correctly.  The AccountStore, BlobStore,
and ActorStore abstractions are complete enough to build handlers on.

The atproto-mst package covers both reading and writing: insert,
fromList, toBlockMap, diff (zipperDiff), and verifyProofs form the
complete MST layer needed by the PDS.

All cryptographic primitives in atproto-haskell-crypto cover key
generation, signing, and verification for both P-256 and secp256k1,
which are required for signing commits and verifying service JWTs.

The atproto-repo package provides typed XRPC bindings for all core
com.atproto.* methods (repo, server, identity, sync, label
namespaces), giving a PDS implementation ready-made request/response
types and codec definitions.

DID resolution in atproto-did (with caching) and handle resolution in
atproto-identity provide the infrastructure needed to verify user keys
and resolve handles.

The atproto-plc package provides the PLC genesis operation creation
needed to register a new did:plc when provisioning an account.

Service auth in atproto-service-auth is ready for the PDS-to-AppView
and PDS-to-Ozone inter-service call pattern.

The XRPC server framework (atproto-xrpc-server) provides WAI
Middleware with parametric handler monad, authentication hooks, and
correct error serialisation.

The XRPC client stack (atproto-xrpc, atproto-xrpc-http) can proxy
unhandled queries to an AppView (the pipethrough pattern) and covers
all outbound calls a PDS makes to upstream services.

Syntax validation in atproto-syntax (TIDs, NSIDs, AT-URIs, handles,
DIDs, record keys) is complete and sufficient for a PDS.

The atproto-firehose consumer and atproto-tap packages are more useful
for relay, indexer, and application roles than for the PDS itself, but
they share infrastructure (CAR parsing, MST verification, DID
resolution) with the PDS subsystems listed above.


Summary of remaining gaps
--------------------------

The following items from the reference packages are not yet implemented:

- Cross-validation of resolved handles against DID document alsoKnownAs
- Configurable DNS timeout in HandleResolver
- exportPrivKey / exportPublicKey in atproto-haskell-crypto
- Test vectors from the AT Protocol specification in crypto tests
- ToJSON instances for Lexicon schema types (atproto-haskell-lexicon)
- Runtime value-level validation of JSON values against Lexicon schemas
- Sequencer (outbound firehose producer)
- OAuth authorisation code grant flow (server-side)
- CAR v2 support
- plc_tombstone operation in atproto-haskell-plc
- PLC audit log verification in atproto-haskell-plc
- PreferenceStore type class
- XRPC handler implementations (the library types exist; the server-side
  glue that calls AccountStore / ActorStore / BlobStore from handlers is
  absent)
- .well-known route serving and relay crawler notification (application
  concerns, trivial once the above are complete)


Minimal PDS Plan
----------------

This section describes the minimum work needed to run a functional
Personal Data Server using the libraries already in this repository.
The scope is deliberately narrow: we do not support anonymous account
creation (all accounts are provisioned by an administrator), and we
do not require zero-trust OAuth (DPoP / full authorisation code grant
can be added later as an upgrade).  Password-based HS256 JWT sessions
cover the initial deployment.

The new work lives in a single new executable package, tentatively
called atproto-pds-server, which imports the library crates and wires
them into a Warp application.

### Phase 1 -- Account provisioning (admin-only)

Goal: an administrator can create a new account for a known user.
This requires:

1. A signed did:plc genesis operation.
   - Generate a fresh secp256k1 or P-256 rotation key and an atproto
     signing key.
   - Build an UnsignedPlcOp (atproto-plc): rotation keys, atproto
     verification method, at://<handle> alsoKnownAs, atproto_pds
     service pointing at this server's URL.
   - Call signPlcOp and plcOpDid to produce the DID.
   - Call submitPlcOp to register it with plc.directory (or a local
     PLC instance for testing).

2. AccountStore initialisation.
   - Construct an Account value and call createAccount on the chosen
     backend (FileAccountStore for production).
   - Call storePassword with a bcrypt hash of the initial password.

3. Repo initialisation.
   - Open an ActorStore via the FileBackend for the new DID.
   - Call initRepo with the actor's signing key to create the empty
     signed repository.

The admin provisioning step can initially be a simple command-line
tool (or a protected HTTP admin endpoint) -- it does not need to be
an XRPC endpoint since we are not accepting self-service signup.

### Phase 2 -- Session management XRPC endpoints

Implement the following com.atproto.server.* handlers:

- createSession: accepts identifier (handle or DID) and password.
  1. Resolve the DID for the handle (atproto-identity or direct DB
     lookup).
  2. Call getPasswordHash and checkPassword.
  3. Call createAccessToken and createRefreshToken.
  4. Call storeRefreshToken.
  5. Return the typed CreateSessionResponse (atproto-repo binding).

- refreshSession: accepts a refresh token in the Authorization header
  or request body.
  1. Call verifyRefreshToken (signature + expiry).
  2. Call getRefreshToken (server-side revocation check).
  3. Call revokeRefreshToken on the old jti.
  4. Issue new access and refresh tokens, store the new refresh token.

- deleteSession (logout): call revokeRefreshToken for the presented
  jti.

- getSession: verify the access token, return DID and handle.

- describeServer: return the server's DID, handle, and capability
  flags.  No auth required.

Authentication hook: wire verifyAccessToken into the XRPC server's
AuthVerifier so that all authenticated endpoints automatically check
the HS256 access token before dispatching.

### Phase 3 -- Repository XRPC endpoints

Implement the following com.atproto.repo.* handlers (each requires
a valid access token via the auth hook from Phase 2):

- createRecord: validate collection NSID, deserialise the request body,
  call createRecord on the ActorStore, return AT-URI and commit CID.
- putRecord: call applyWrites (Update op).
- deleteRecord: call deleteRecord on the ActorStore.
- getRecord: call getRecord on the ActorStore.
- listRecords: call listRecords, apply cursor/limit pagination.
- describeRepo: call getAccount for handle; return handle, DID, and
  collection list derived from the MST keys.
- applyWrites: decode the batch, call applyWrites on the ActorStore.
- uploadBlob: stream the request body through BlobStore.putTemp, then
  BlobStore.makePermanent after computing the CID.
- getBlob: stream via BlobStore.streamBytes.

### Phase 4 -- Identity and sync endpoints

- com.atproto.identity.resolveHandle: call the HandleResolver.
- com.atproto.identity.resolveDid: call the DidResolver.
- com.atproto.sync.getRepo: call exportRepoCar and stream the result.
- com.atproto.sync.getLatestCommit: return head CID and rev from the
  ActorStore.

### Phase 5 -- Well-known routes

Serve two routes outside the /xrpc path:

- GET /.well-known/atproto-did -- return the server's own DID as plain
  text (used for did:web and handle verification).
- GET /xrpc/com.atproto.identity.resolveHandle with ?handle=<this-host>
  already handles the handle-to-DID resolution path.

These are trivial WAI route additions alongside the xrpcMiddleware.

### Phase 6 -- Relay notification (optional)

After each commit, call com.atproto.sync.notifyOfUpdate on configured
relay endpoints using the atproto-xrpc-http client.  This is a
fire-and-forget POST; failures should be logged but not surface to
the caller.

### What is explicitly out of scope

- Zero-trust / anonymous account creation: the server will refuse
  com.atproto.server.createAccount from clients.  Account creation
  goes through the admin provisioning tool only.
- Full OAuth authorisation code grant (DPoP, consent UI, PAR): the
  atproto-oauth-provider token-verification layer is available and can
  be added later, but the initial deployment uses HS256 JWTs.
- Email verification and password-reset flows: the AccountStore has
  fields for email but no mailer subsystem; out of scope for now.
- Sequencer / WebSocket firehose: commits will be stored but not
  broadcast; relays that discover the PDS via notifyOfUpdate will
  crawl via com.atproto.sync.getRepo instead.
- App password management: the single password per account is
  sufficient for the initial deployment.
- Multi-tenant / hosted PDS: the initial target is a single-tenant
  self-hosted instance with a small number of admin-provisioned accounts.
