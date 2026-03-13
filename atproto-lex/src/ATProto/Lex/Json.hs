-- | JSON bridge for 'Codec'.
--
-- Converts between Haskell types and JSON by going through 'LexValue':
--
-- @
-- Haskell type a  ←→  LexValue  ←→  Aeson.Value  ←→  JSON bytes
-- @
module ATProto.Lex.Json
  ( -- * Aeson bridge
    toJSON
  , fromJSON
    -- * Byte-string bridge
  , encode
  , decode
  ) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import ATProto.Ipld.Json  (encodeLexJson, jsonToLexValue, lexValueToJson)
import ATProto.Lex.Codec  (Codec (..), LexError (..))

-- | Encode a value to an Aeson 'Aeson.Value' using its codec.
toJSON :: Codec a -> a -> Aeson.Value
toJSON c = lexValueToJson . writer c

-- | Decode a value from an Aeson 'Aeson.Value' using its codec.
fromJSON :: Codec a -> Aeson.Value -> Either LexError a
fromJSON c v =
    case jsonToLexValue v of
        Left  err -> Left (InvalidValue "JSON" (T.pack err))
        Right lv  -> decoder c lv

-- | Encode a value to a JSON 'BL.ByteString'.
encode :: Codec a -> a -> BL.ByteString
encode c = encodeLexJson . writer c

-- | Decode a value from a JSON 'BL.ByteString'.
decode :: Codec a -> BL.ByteString -> Either String a
decode c bs =
    case Aeson.eitherDecode bs of
        Left  err -> Left err
        Right v   ->
            case jsonToLexValue v of
                Left  err -> Left err
                Right lv  ->
                    case decoder c lv of
                        Left  err -> Left (show err)
                        Right a   -> Right a
