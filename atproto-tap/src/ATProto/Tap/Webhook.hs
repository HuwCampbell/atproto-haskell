-- | Webhook authentication helpers.
--
-- Provides a simple Basic-auth check for webhook receivers that need
-- to verify incoming requests from Tap.
module ATProto.Tap.Webhook
  ( assureAdminAuth
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Text.Encoding as TE

-- | Verify that a Basic auth header value matches the expected admin password.
--
-- Expects the raw @Authorization@ header value (e.g. @\"Basic YWRtaW46c2VjcmV0\"@).
-- Returns 'Right ()' if the credentials match @admin:\<password\>@, or
-- 'Left' with an error message otherwise.
assureAdminAuth :: T.Text -> BS.ByteString -> Either String ()
assureAdminAuth expectedPassword authHeader =
  case BS.stripPrefix "Basic " authHeader of
    Nothing -> Left "Missing Basic auth prefix"
    Just encoded ->
      let decoded = decodeBase64Simple encoded
      in case decoded of
           Nothing -> Left "Invalid Base64 encoding"
           Just cred ->
             let expected = TE.encodeUtf8 ("admin:" <> expectedPassword)
             in if cred == expected
                  then Right ()
                  else Left "Invalid admin credentials"

-- Minimal Base64 decoder for ASCII credentials.
decodeBase64Simple :: BS.ByteString -> Maybe BS.ByteString
decodeBase64Simple bs =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      indexOf w = case BS.elemIndex w alphabet of
                    Just i  -> Just (fromIntegral i)
                    Nothing -> if w == 0x3D then Just (-1 :: Int) else Nothing
      bytes = BS.unpack bs
      decode4 [a,b,c,d] = do
        ia <- indexOf a
        ib <- indexOf b
        ic <- indexOf c
        id' <- indexOf d
        let b0 = fromIntegral (ia * 4 + ib `div` 16)
        if ic == -1
          then Just [b0]
          else do
            let b1 = fromIntegral ((ib `mod` 16) * 16 + ic `div` 4)
            if id' == -1
              then Just [b0, b1]
              else do
                let b2 = fromIntegral ((ic `mod` 4) * 64 + id')
                Just [b0, b1, b2]
      decode4 _ = Nothing
      chunks [] = Just []
      chunks xs =
        let (chunk, rest) = splitAt 4 xs
        in if length chunk < 4
           then Nothing
           else do
             decoded <- decode4 chunk
             remaining <- chunks rest
             Just (decoded ++ remaining)
  in fmap BS.pack (chunks bytes)
