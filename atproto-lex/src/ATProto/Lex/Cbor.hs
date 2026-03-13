-- | CBOR bridge for 'Codec'.
--
-- Converts between Haskell types and CBOR by going through 'LexValue':
--
-- @
-- Haskell type a  ←→  LexValue  ←→  CBOR.Term  ←→  CBOR bytes
-- @
module ATProto.Lex.Cbor
  ( -- * Term bridge
    toTerm
  , fromTerm
    -- * Byte-string bridge
  , serialise
  , deserialise
  ) where

import qualified Codec.CBOR.Read      as CBOR
import qualified Codec.CBOR.Term      as CBOR
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import ATProto.Ipld.Cbor (decodeLexCbor, encodeLexCbor,
                           lexValueToTerm, termToLexValue)
import ATProto.Lex.Codec (Codec (..), LexError (..))

-- | Encode a value as a CBOR 'CBOR.Term'.
toTerm :: Codec a -> a -> CBOR.Term
toTerm c = lexValueToTerm . writer c

-- | Decode a value from a CBOR 'CBOR.Term'.
fromTerm :: Codec a -> CBOR.Term -> Either LexError a
fromTerm c t =
    case termToLexValue t of
        Left  err -> Left (InvalidValue "CBOR" (T.pack err))
        Right lv  -> decoder c lv

-- | Encode a value to canonical DAG-CBOR bytes.
serialise :: Codec a -> a -> BL.ByteString
serialise c = encodeLexCbor . writer c

-- | Decode a value from DAG-CBOR bytes.
deserialise :: Codec a -> BL.ByteString -> Either CBOR.DeserialiseFailure a
deserialise c bs =
    case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
        Left  err       -> Left err
        Right (_, term) ->
            case termToLexValue term of
                Left  err -> Left (CBOR.DeserialiseFailure 0 err)
                Right lv  ->
                    case decoder c lv of
                        Left  err -> Left (CBOR.DeserialiseFailure 0 (show err))
                        Right a   -> Right a
