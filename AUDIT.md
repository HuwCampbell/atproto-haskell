API Alignment Audit
===================

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
Haskell with no additional dependencies.

The reference package also exports utility functions for importing raw
private keys and for parsing compact signatures.  importPrivKey is
present; compact signature parsing is handled implicitly within
verify.  The reference package's exportPrivKey / exportPublicKey
functions are not yet implemented.

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

Two resolvers are implemented: PlcResolver (did:plc via the PLC
directory at plc.directory) and WebResolver (did:web via the
.well-known/did.json endpoint).  Both match the reference behaviour,
including the AT Protocol restriction that did:web identifiers
containing path components are rejected.

The reference identity package combines DID and handle resolution in
a single IdResolver class.  The Haskell design keeps them in separate
packages (atproto-haskell-did for DID documents, atproto-haskell-identity
for handle resolution), which is a deliberate decomposition into smaller
units of responsibility.

The reference also caches resolved documents with configurable TTLs.
No caching layer exists in atproto-haskell-did; this is a gap.


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


atproto-haskell-lexicon  (@atproto/lexicon)
--------------------------------------------

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

None of these are implemented in atproto-haskell-lexicon.  The current
package handles only parsing of schema documents, not validation of
record values against those schemas.


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


atproto-haskell-repo  (portions of @atproto/api)
--------------------------------------------------

The package provides a typed binding for com.atproto.repo.listRecords.
ListRecordsParams covers all five query parameters from the Lexicon
(repo, collection, limit, cursor, reverse).  ListRecordsResponse and
RepoRecord parse the JSON response body.  The listRecords function
works with any XrpcClient backend.

No other com.atproto.repo methods are implemented (createRecord,
putRecord, deleteRecord, getRecord, describeRepo, uploadBlob).  These
are straightforward additions following the same pattern.


Summary of gaps
---------------

The following items from the reference packages are not yet implemented:

- DID document caching with configurable TTL
- Cross-validation of resolved handles against DID document alsoKnownAs
- Configurable DNS timeout in HandleResolver
- exportPrivKey / exportPublicKey in atproto-haskell-crypto
- Test vectors from the AT Protocol specification in crypto tests
- Lexicon schema validation (checking record values against schemas)
- ToJSON instances for Lexicon types
- Remaining com.atproto.repo.* XRPC bindings
- @atproto/oauth-client (planned; see docs/oauth-plan.md)


atproto-haskell-car  (@atproto/repo — CAR parsing)
----------------------------------------------------

CAR v1 (Content Addressable aRchive) parsing is fully implemented.
The parser covers the varint-length-prefixed header (DAG-CBOR map with
version and roots fields), CBOR tag-42 CID encoding/decoding, and the
block sequence.  Binary CIDv1 parsing dynamically determines CID length
by reading version, codec, and multihash header varints.  Display uses
multibase base32lower (prefix 'b'), compatible with the existing
string-based ATProto.Ipld.Value.Cid type.

readCar handles zero or more roots; readCarWithRoot enforces exactly
one root.  The BlockMap type (Map CidBytes ByteString) is the central
data structure consumed by atproto-haskell-mst.

Gap: no CAR v2 support (the reference repo package supports both).


atproto-haskell-mst  (@atproto/repo — MST)
-------------------------------------------

Merkle Search Tree operations are fully implemented:

  - Node decoding: NodeData and TreeEntry from DAG-CBOR, including
    nullable tag-42 CID fields for left subtrees and right subtrees.
  - Layer computation: leadingZerosOnHash counts leading 2-bit zero
    pairs in the SHA-256 digest, matching the reference exactly.
    Test vectors from the TypeScript suite are verified.
  - Point lookup: get performs a correct flat-entry interleaved walk
    (leftmost subtree, leaf, right subtree for each entry) and
    descends into subtrees as needed.
  - Tree diff: mstDiff collects all leaves from both trees in sorted
    order and computes WCreate/WUpdate/WDelete descriptors.  Equal
    subtree CIDs are short-circuited (subtrees not re-walked).
  - Proof verification: verifyProofs checks each RecordOp against the
    live MST, comparing found CIDs with claimed CIDs.

Gap: the diff implementation walks full leaf lists rather than doing
a true simultaneous walk with CID-equality subtree pruning at the
node level.  For large trees with small diffs this is less efficient
than the reference, but produces correct results.

Gap: test coverage uses single-node trees only.  The 11-key known-
root-CID test vectors from the TypeScript mst.test.ts are not yet
verified against hand-crafted fixtures.


atproto-haskell-repo-verify  (@atproto/repo — commit verification)
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
  - verifyCommitCar: full pipeline — parse CAR, decode commit, assert
    DID, extract key, verify sig, verify MST proofs, return diff.

Gap: encodeUnsignedCommit always includes the prev field (as CBOR
null when absent).  Legacy v2 commits that omit prev entirely will
produce a signature mismatch.  V3 commits (current standard) always
include prev and are handled correctly.

Gap: no caching of resolved DID documents.  The authenticated
firehose client calls afcResolveDid twice on key-rotation failures;
callers must implement caching to avoid excessive network requests.


atproto-haskell-firehose  (@atproto/sync — Firehose)
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
