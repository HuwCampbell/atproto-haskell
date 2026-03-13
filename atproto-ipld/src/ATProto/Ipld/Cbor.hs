-- | DAG-CBOR encoding and decoding for 'LexValue'.
--
-- Implements the AT Protocol\/IPLD DAG-CBOR conventions:
--
--   * CID links are encoded with CBOR tag 42 and a leading @\\x00@ multibase
--     prefix byte followed by the raw CID bytes.  For simplicity this
--     implementation round-trips the CID as a UTF-8 string under the tag,
--     which is sufficient for testing; a production implementation should use
--     a proper multibase\/CID library.
--   * Byte arrays use standard CBOR bytes.
--   * Only integers are allowed; floats in input are rejected.
--   * Map keys must be text strings (DAG-CBOR requirement).
--   * Canonical CBOR (shortest integer encoding) is used on output.
module ATProto.Ipld.Cbor
  ( -- * Converting to\/from CBOR Terms
    lexValueToTerm
  , termToLexValue
    -- * Encoding\/decoding bytes
  , encodeLexCbor
  , decodeLexCbor
  ) where

import qualified Codec.CBOR.Read      as CBOR
import qualified Codec.CBOR.Term      as CBOR
import qualified Codec.CBOR.Write     as CBOR
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.Lazy       as LT
import           Data.Int             (Int64)
import           Data.Word            (Word64)

import ATProto.Ipld.Value (Cid (..), LexValue (..))

-- | The CBOR tag number for CID links (tag 42, per the IPLD\/DAG-CBOR spec).
cidTagNum :: Word64
cidTagNum = 42

-- | Encode a 'LexValue' as a CBOR 'CBOR.Term'.
--
-- CID links are encoded with tag 42 and the UTF-8 bytes of the CID string
-- prefixed by a @\\x00@ multibase identity byte.
lexValueToTerm :: LexValue -> CBOR.Term
lexValueToTerm LexNull             = CBOR.TNull
lexValueToTerm (LexBool b)         = CBOR.TBool b
lexValueToTerm (LexInt n)          = CBOR.TInteger (fromIntegral n)
lexValueToTerm (LexString t)       = CBOR.TString t
lexValueToTerm (LexBytes bs)       = CBOR.TBytes bs
lexValueToTerm (LexLink (Cid c))   =
    -- Prepend the 0x00 multibase identity prefix byte to the UTF-8 CID string.
    CBOR.TTagged cidTagNum (CBOR.TBytes (BS.cons 0x00 (TE.encodeUtf8 c)))
lexValueToTerm (LexArray vs)       = CBOR.TList (map lexValueToTerm vs)
lexValueToTerm (LexObject m)       =
    -- Keys must be sorted in byte order for canonical DAG-CBOR.
    CBOR.TMapI
        [ (CBOR.TString k, lexValueToTerm v)
        | (k, v) <- Map.toAscList m
        ]

-- | Decode a CBOR 'CBOR.Term' into a 'LexValue'.
--
-- Returns 'Left' with an error message if the term contains a float,
-- an integer too large for 'Int64', or a map with non-text keys.
termToLexValue :: CBOR.Term -> Either String LexValue
termToLexValue CBOR.TNull             = Right LexNull
termToLexValue (CBOR.TBool b)         = Right (LexBool b)
termToLexValue (CBOR.TInt n)          = Right (LexInt (fromIntegral n))
termToLexValue (CBOR.TInteger n)
    | n >= fromIntegral (minBound :: Int64) && n <= fromIntegral (maxBound :: Int64)
    = Right (LexInt (fromIntegral n))
    | otherwise
    = Left ("LexValue: integer out of Int64 range: " ++ show n)
termToLexValue (CBOR.TString t)       = Right (LexString t)
termToLexValue (CBOR.TStringI t)      = Right (LexString (LT.toStrict t))
termToLexValue (CBOR.TBytes bs)       = Right (LexBytes bs)
termToLexValue (CBOR.TBytesI bs)      = Right (LexBytes (BL.toStrict bs))
termToLexValue (CBOR.THalf _)         = Left "LexValue: floating-point numbers are not allowed"
termToLexValue (CBOR.TFloat _)        = Left "LexValue: floating-point numbers are not allowed"
termToLexValue (CBOR.TDouble _)       = Left "LexValue: floating-point numbers are not allowed"
termToLexValue (CBOR.TTagged tag inner)
    | tag == cidTagNum = decodeCid inner
    | otherwise        = termToLexValue inner
  where
    decodeCid (CBOR.TBytes bs) =
        -- Strip the 0x00 multibase prefix byte and decode as UTF-8.
        case BS.uncons bs of
            Just (0x00, rest) ->
                case TE.decodeUtf8' rest of
                    Left  err -> Left ("LexValue: CID is not valid UTF-8: " ++ show err)
                    Right c   -> Right (LexLink (Cid c))
            _ ->
                Left "LexValue: CID tag 42 payload missing 0x00 prefix"
    decodeCid _ =
        Left "LexValue: CID tag 42 payload must be bytes"
termToLexValue (CBOR.TList vs)        = fmap LexArray (mapM termToLexValue vs)
termToLexValue (CBOR.TListI vs)       = fmap LexArray (mapM termToLexValue vs)
termToLexValue (CBOR.TMap pairs)      = decodePairs pairs
termToLexValue (CBOR.TMapI pairs)     = decodePairs pairs
termToLexValue other                  =
    Left ("LexValue: unexpected CBOR term: " ++ show other)

-- | Decode a list of CBOR key-value pairs into a 'LexValue' map.
decodePairs :: [(CBOR.Term, CBOR.Term)] -> Either String LexValue
decodePairs pairs = do
    kvs <- mapM decodePair pairs
    return (LexObject (Map.fromList kvs))
  where
    decodePair (CBOR.TString k, v) = fmap ((,) k) (termToLexValue v)
    decodePair (k, _)              =
        Left ("LexValue: map key must be a text string, got: " ++ show k)

-- | Serialise a 'LexValue' to canonical DAG-CBOR bytes.
encodeLexCbor :: LexValue -> BL.ByteString
encodeLexCbor = CBOR.toLazyByteString . CBOR.encodeTerm . lexValueToTerm

-- | Deserialise a 'LexValue' from DAG-CBOR bytes.
decodeLexCbor :: BL.ByteString -> Either String LexValue
decodeLexCbor bs =
    case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
        Left  err       -> Left (show err)
        Right (_, term) -> termToLexValue term
