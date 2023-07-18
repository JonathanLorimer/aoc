module Y2015.D08 where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, choice, try)
import Text.Megaparsec.Char
import Text.Megaparsec (between)
import Control.Monad (void)
import Numeric (readHex)
import Data.Char (chr)
import Data.Foldable

 -- \\ (which represents a single backslash)
backslash :: Parsec Void Text Char
backslash = char '\\' >> char '\\'

 -- \" (which represents a lone double-quote character)
quote :: Parsec Void Text Char
quote = char '\\' >> char '"'

 -- \x plus two hexadecimal characters (which represents a single character by char code)
hexChar :: Parsec Void Text Char
hexChar = do
  void $ char '\\' >> char 'x'
  a <- hexDigitChar
  b <- hexDigitChar
  pure . chr . fst . head . readHex $ a:b:[]

listChar :: Parsec Void Text Char
listChar = choice 
  [ try backslash
  , try quote
  , try hexChar
  , lowerChar
  ]

listItem :: Parsec Void Text String
listItem = between (char '"') (char '"') $ many listChar

-- Part 2
expandBackslash :: Parsec Void Text String
expandBackslash = char '\\' >> pure "\\\\"

 -- \" (which represents a lone double-quote character)
expandQuote :: Parsec Void Text String
expandQuote = char '"' >> pure "\\\""

 -- \x plus two hexadecimal characters (which represents a single character by char code)
expandHexChar :: Parsec Void Text String
expandHexChar = do
  void $ char 'x'
  a <- hexDigitChar
  b <- hexDigitChar
  pure $ 'x':a:b:[]

expandLowerChar :: Parsec Void Text String 
expandLowerChar = fmap (:[]) lowerChar

expandListChar :: Parsec Void Text String
expandListChar = choice 
  [ try expandBackslash
  , try expandQuote
  , try expandHexChar
  , expandLowerChar
  ]

expandListItem :: Parsec Void Text String
expandListItem = ("\"" <>) . (<> "\"") . fold <$> many expandListChar
