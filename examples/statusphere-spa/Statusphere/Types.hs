-- | Shared types and Lex codecs for the Statusphere SPA example.
--
-- Defines the @xyz.statusphere.status@ record type and the custom XRPC
-- response types using the Lex codec system.
module Statusphere.Types
  ( -- * Status record
    StatusView (..)
  , statusViewCodec
    -- * XRPC response types
  , StatusEntry (..)
  , statusEntryCodec
  , GetStatusesResponse (..)
  , getStatusesResponseCodec
  , SetStatusRequest (..)
  , setStatusRequestCodec
  , SetStatusResponse (..)
  , setStatusResponseCodec
  ) where

import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec

-- ---------------------------------------------------------------------------
-- Status record (stored on the user's PDS)
-- ---------------------------------------------------------------------------

-- | The on-wire record for @xyz.statusphere.status@.
data StatusView = StatusView
  { svStatus    :: T.Text
    -- ^ The emoji status.
  , svCreatedAt :: T.Text
    -- ^ ISO-8601 creation timestamp.
  } deriving (Eq, Show)

-- | Codec for @xyz.statusphere.status@.
statusViewCodec :: Codec StatusView
statusViewCodec =
    Codec.record "xyz.statusphere.status" $
        StatusView
            <$> Codec.requiredField "status"    Codec.text     svStatus
            <*> Codec.requiredField "createdAt" Codec.datetime svCreatedAt

-- ---------------------------------------------------------------------------
-- XRPC: xyz.statusphere.getStatuses
-- ---------------------------------------------------------------------------

-- | A single status entry in the getStatuses response.
data StatusEntry = StatusEntry
  { seUri       :: T.Text
    -- ^ AT-URI of the record.
  , seAuthorDid :: T.Text
    -- ^ DID of the author.
  , seStatus    :: T.Text
    -- ^ The emoji status.
  , seCreatedAt :: T.Text
    -- ^ When the status was created.
  , seIndexedAt :: T.Text
    -- ^ When the status was indexed locally.
  } deriving (Eq, Show)

-- | Codec for a single status entry.
statusEntryCodec :: Codec StatusEntry
statusEntryCodec =
    Codec.record "xyz.statusphere.getStatuses#statusEntry" $
        StatusEntry
            <$> Codec.requiredField "uri"       Codec.atUri    seUri
            <*> Codec.requiredField "authorDid" Codec.did      seAuthorDid
            <*> Codec.requiredField "status"    Codec.text     seStatus
            <*> Codec.requiredField "createdAt" Codec.datetime seCreatedAt
            <*> Codec.requiredField "indexedAt" Codec.datetime seIndexedAt

-- | Response for @xyz.statusphere.getStatuses@.
data GetStatusesResponse = GetStatusesResponse
  { gsrStatuses :: [StatusEntry]
  } deriving (Eq, Show)

-- | Codec for the getStatuses response.
getStatusesResponseCodec :: Codec GetStatusesResponse
getStatusesResponseCodec =
    Codec.record "xyz.statusphere.getStatuses#response" $
        GetStatusesResponse
            <$> Codec.requiredField "statuses" (Codec.array statusEntryCodec) gsrStatuses

-- ---------------------------------------------------------------------------
-- XRPC: xyz.statusphere.setStatus
-- ---------------------------------------------------------------------------

-- | Request for @xyz.statusphere.setStatus@.
data SetStatusRequest = SetStatusRequest
  { ssrStatus :: T.Text
    -- ^ The emoji to set.
  } deriving (Eq, Show)

-- | Codec for the setStatus request body.
setStatusRequestCodec :: Codec SetStatusRequest
setStatusRequestCodec =
    Codec.record "xyz.statusphere.setStatus#request" $
        SetStatusRequest
            <$> Codec.requiredField "status" Codec.text ssrStatus

-- | Response for @xyz.statusphere.setStatus@.
data SetStatusResponse = SetStatusResponse
  { ssUri    :: T.Text
    -- ^ AT-URI of the written record.
  , ssStatus :: T.Text
    -- ^ The status that was set.
  } deriving (Eq, Show)

-- | Codec for the setStatus response.
setStatusResponseCodec :: Codec SetStatusResponse
setStatusResponseCodec =
    Codec.record "xyz.statusphere.setStatus#response" $
        SetStatusResponse
            <$> Codec.requiredField "uri"    Codec.atUri ssUri
            <*> Codec.requiredField "status" Codec.text  ssStatus
