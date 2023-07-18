module Y2015.D04 where
import Data.ByteString hiding (unpack, toStrict)
import Numeric.Natural (Natural)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, lazyByteStringHex)
import Crypto.Hash.MD5 (hash)


hashAttempt :: Text -> Natural -> Text
hashAttempt prefix n = 
  let bs = encodeUtf8 $ prefix <> T.pack (show n)
   in toStrict 
    . decodeUtf8 
    . toLazyByteString 
    . lazyByteStringHex 
    . fromStrict 
    . hash
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
          h = if T.take lengthPrefix attempt == prefix then Just (n, attempt) else Nothing
       in go (n + 1) h 
