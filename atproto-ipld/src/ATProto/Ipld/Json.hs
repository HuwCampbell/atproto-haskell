-- | DAG-JSON encoding and decoding for 'LexValue'.
--
-- Implements the AT Protocol\/IPLD DAG-JSON conventions:
--
--   * CID links are encoded as @{\"$link\": \<cid-string\>}@
--   * Byte arrays are encoded as @{\"$bytes\": \<base64url-string\>}@
--   * Only integers are allowed as numbers (no floats)
--   * @null@ maps to 'LexNull'; boolean maps to 'LexBool'
module ATProto.Ipld.Json
  ( -- * Converting to\/from Aeson
    lexValueToJson
  , jsonToLexValue
    -- * Encoding\/decoding bytes
  , encodeLexJson
  , decodeLexJson
  ) where

import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Key          as Key
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Scientific         as Scientific
import qualified Data.Text.Encoding      as TE
import qualified Data.Vector             as V

import ATProto.Ipld.Value (Cid (..), LexValue (..))

-- | Encode a 'LexValue' as an Aeson 'Aeson.Value' using DAG-JSON conventions.
--
-- CID links become @{\"$link\": \<string\>}@ and byte arrays become
-- @{\"$bytes\": \<base64url\>}@.
lexValueToJson :: LexValue -> Aeson.Value
lexValueToJson LexNull         = Aeson.Null
lexValueToJson (LexBool b)     = Aeson.Bool b
lexValueToJson (LexInt  n)     = Aeson.Number (fromIntegral n)
lexValueToJson (LexString t)   = Aeson.String t
lexValueToJson (LexBytes bs)   =
    Aeson.object [ "$bytes" Aeson..= TE.decodeUtf8 (B64.encode bs) ]
lexValueToJson (LexLink (Cid c)) =
    Aeson.object [ "$link" Aeson..= c ]
lexValueToJson (LexArray vs)   =
    Aeson.Array (V.fromList (map lexValueToJson vs))
lexValueToJson (LexObject m)   =
    Aeson.Object (KM.fromMapText (Map.map lexValueToJson m))

-- | Decode an Aeson 'Aeson.Value' into a 'LexValue'.
--
-- Returns 'Left' with an error message if the JSON contains a floating-point
-- number or any other value not representable in the AT Protocol data model.
jsonToLexValue :: Aeson.Value -> Either String LexValue
jsonToLexValue Aeson.Null       = Right LexNull
jsonToLexValue (Aeson.Bool b)   = Right (LexBool b)
jsonToLexValue (Aeson.String t) = Right (LexString t)
jsonToLexValue (Aeson.Number n) =
    if Scientific.isInteger n
        then case Scientific.toBoundedInteger n of
            Nothing -> Left "LexValue: integer out of Int64 range"
            Just i  -> Right (LexInt i)
        else Left "LexValue: floating-point numbers are not allowed"
jsonToLexValue (Aeson.Array vs) = do
    vs' <- mapM jsonToLexValue (V.toList vs)
    return (LexArray vs')
jsonToLexValue (Aeson.Object km) =
    -- Check for special singleton objects first.
    case (KM.lookup "$link" km, KM.lookup "$bytes" km) of
        (Just (Aeson.String c), _) | KM.size km == 1 ->
            Right (LexLink (Cid c))
        (_, Just (Aeson.String b64)) | KM.size km == 1 ->
            case B64.decode (TE.encodeUtf8 b64) of
                Left  err -> Left ("LexValue: invalid base64 in $bytes: " ++ err)
                Right bs  -> Right (LexBytes bs)
        _ -> do
            let pairs = KM.toList km
            pairs' <- mapM (\(k, v) -> fmap ((,) (Key.toText k)) (jsonToLexValue v)) pairs
            return (LexObject (Map.fromList pairs'))

-- | Serialise a 'LexValue' to a JSON 'BL.ByteString'.
encodeLexJson :: LexValue -> BL.ByteString
encodeLexJson = Aeson.encode . lexValueToJson

-- | Deserialise a 'LexValue' from a JSON 'BL.ByteString'.
decodeLexJson :: BL.ByteString -> Either String LexValue
decodeLexJson bs =
    case Aeson.eitherDecode bs of
        Left  err -> Left err
        Right v   -> jsonToLexValue v
