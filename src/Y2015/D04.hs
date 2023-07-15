module Y2015.D04 where
import Data.ByteString hiding (unpack, toStrict)
import Crypto.Hash
import Numeric.Natural (Natural)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteArray (unpack)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, word8HexFixed, lazyByteStringHex)


hashAttempt :: Text -> Natural -> Text
hashAttempt prefix n = 
  let bs = encodeUtf8 $ prefix <> T.pack (show n)
   in toStrict 
    . decodeUtf8 
    . toLazyByteString 
    . lazyByteStringHex 
    . fromStrict 
    . pack 
    . unpack 
    . hashWith MD5 
    $ bs

findHashWithPrefix :: Text -> Text -> (Natural, Text)
findHashWithPrefix toHash prefix  = go 0 Nothing
  where
    lengthPrefix :: Int
    lengthPrefix = T.length prefix

    go :: Natural -> Maybe (Natural, Text) -> (Natural, Text)
    go _ (Just res) = res
    go n Nothing = 
      let attempt = hashAttempt toHash n
          hash = if T.take lengthPrefix attempt == prefix then Just (n, attempt) else Nothing
       in go (n + 1) hash 
