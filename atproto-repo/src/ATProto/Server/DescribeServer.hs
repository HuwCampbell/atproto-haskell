-- | Typed binding for @com.atproto.server.describeServer@.
--
-- Describes the server's account creation requirements and capabilities.
-- Implemented by PDS.
--
-- The Lexicon for this method is at
-- <https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/server/describeServer.json>.
module ATProto.Server.DescribeServer
  ( -- * Response
    DescribeServerResponse (..)
  , DescribeServerLinks (..)
  , DescribeServerContact (..)
    -- * Codecs
  , describeServerResponseCodec
  , describeServerLinksCodec
  , describeServerContactCodec
    -- * Client function
  , describeServer
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import           ATProto.Lex.Codec       (Codec)
import qualified ATProto.Lex.Codec       as Codec
import qualified ATProto.Lex.Json        as LexJson
import           ATProto.XRPC.Client     (XrpcClient (..))
import           ATProto.XRPC.Types      (XrpcError (..), XrpcMethod (..), XrpcRequest (..), XrpcResponse (..))

-- ---------------------------------------------------------------------------
-- Response
-- ---------------------------------------------------------------------------

-- | Response from @com.atproto.server.describeServer@.
data DescribeServerResponse = DescribeServerResponse
  { dsrDid                       :: T.Text
    -- ^ The DID of the server.
  , dsrAvailableUserDomains      :: [T.Text]
    -- ^ List of domain suffixes that can be used in account handles.
  , dsrInviteCodeRequired        :: Maybe Bool
    -- ^ If true, an invite code must be supplied to create an account.
  , dsrPhoneVerificationRequired :: Maybe Bool
    -- ^ If true, a phone verification token must be supplied.
  , dsrLinks                     :: Maybe DescribeServerLinks
    -- ^ URLs of service policy documents.
  , dsrContact                   :: Maybe DescribeServerContact
    -- ^ Contact information.
  } deriving (Eq, Show)

-- | Service policy links.
--
-- Corresponds to @com.atproto.server.describeServer#links@.
data DescribeServerLinks = DescribeServerLinks
  { dslPrivacyPolicy  :: Maybe T.Text
    -- ^ URL to the privacy policy.
  , dslTermsOfService :: Maybe T.Text
    -- ^ URL to the terms of service.
  } deriving (Eq, Show)

-- | Contact information for the server.
--
-- Corresponds to @com.atproto.server.describeServer#contact@.
newtype DescribeServerContact = DescribeServerContact
  { dscEmail :: Maybe T.Text
    -- ^ Contact email address.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Codecs
-- ---------------------------------------------------------------------------

-- | Codec for @com.atproto.server.describeServer#links@.
describeServerLinksCodec :: Codec DescribeServerLinks
describeServerLinksCodec =
    Codec.record "com.atproto.server.describeServer#links" $
        DescribeServerLinks
            <$> Codec.optionalField "privacyPolicy"  Codec.uri dslPrivacyPolicy
            <*> Codec.optionalField "termsOfService"  Codec.uri dslTermsOfService

-- | Codec for @com.atproto.server.describeServer#contact@.
describeServerContactCodec :: Codec DescribeServerContact
describeServerContactCodec =
    Codec.record "com.atproto.server.describeServer#contact" $
        DescribeServerContact
            <$> Codec.optionalField "email" Codec.text dscEmail

-- | Codec for the @describeServer@ response body.
describeServerResponseCodec :: Codec DescribeServerResponse
describeServerResponseCodec =
    Codec.record "com.atproto.server.describeServer#response" $
        DescribeServerResponse
            <$> Codec.requiredField "did"                       Codec.did                           dsrDid
            <*> Codec.requiredField "availableUserDomains"      (Codec.array Codec.text)            dsrAvailableUserDomains
            <*> Codec.optionalField "inviteCodeRequired"        Codec.bool                          dsrInviteCodeRequired
            <*> Codec.optionalField "phoneVerificationRequired" Codec.bool                          dsrPhoneVerificationRequired
            <*> Codec.optionalField "links"                     describeServerLinksCodec             dsrLinks
            <*> Codec.optionalField "contact"                   describeServerContactCodec           dsrContact

-- ---------------------------------------------------------------------------
-- Client function
-- ---------------------------------------------------------------------------

-- | Call @com.atproto.server.describeServer@ using the given XRPC client.
describeServer
  :: XrpcClient c
  => c
  -> IO (Either XrpcError DescribeServerResponse)
describeServer client = do
  result <- runXrpc client XrpcRequest
    { xrpcReqMethod  = XrpcQuery
    , xrpcReqNsid    = "com.atproto.server.describeServer"
    , xrpcReqParams  = Map.empty
    , xrpcReqBody    = Nothing
    , xrpcReqHeaders = Map.empty
    }
  case result of
    Left  err  -> return (Left err)
    Right resp -> return (parseResponse (xrpcRespBody resp))

parseResponse :: BL.ByteString -> Either XrpcError DescribeServerResponse
parseResponse body =
  case LexJson.decode describeServerResponseCodec body of
    Right r  -> Right r
    Left msg -> Left XrpcError
        { xrpcErrError   = "ParseError"
        , xrpcErrMessage = Just (T.pack msg)
        , xrpcErrStatus  = 200
        , xrpcErrHeaders = Map.empty
        }
