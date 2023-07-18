module Y2015.D08Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D08 
import Utils (assertJust, assertRight)
import Text.Megaparsec (parseMaybe, runParser)
import Data.Monoid (Sum(..))
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Matchsticks pt.1" $ do
    it "\"\" should parse as the empty string" $ do
      res <- assertRight $ runParser listItem "inline" "\"\""
      res `shouldBe` ""
    it "\"aaa\\\"aaa\" should parse as aaa\"aaa" $ do
      res <- assertRight $ runParser listItem "inline" "\"aaa\\\"aaa\""
      res `shouldBe` "aaa\"aaa"
    it "\"\\x27\" should parse as '" $ do
      res <- assertRight $ runParser listItem "inline" "\"\\x27\""
      res `shouldBe` "'"
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D08.txt"
      parsedLines <- assertJust $ traverse (parseMaybe listItem) input
      let codeChars = foldMap (Sum . T.length) input
          memoryChars = foldMap (Sum . length) parsedLines
      codeChars - memoryChars `shouldBe` 1371
      
  describe "Matchsticks pt.2" $ do
    it "\"\" should get encoded as \"\\\"\\\"\" " $ do
      res <- assertRight $ runParser expandListItem "inline" "\"\""
      res `shouldBe` "\"\\\"\\\"\""
    it "\"abc\" should get encoded as \"\\\"abc\\\"\"" $ do
      res <- assertRight $ runParser expandListItem "inline" "\"abc\""
      res `shouldBe` "\"\\\"abc\\\"\""
    it "\"aaa\\\"aaa\" should get encoded as \"\\\"aaa\\\\\\\"aaa\\\"\"" $ do
      res <- assertRight $ runParser expandListItem "inline" "\"aaa\\\"aaa\""
      res `shouldBe` "\"\\\"aaa\\\\\\\"aaa\\\"\""
    it "\"\\x27\" should get encoded as \"\\\"\\\\x27\\\"\"" $ do
      res <- assertRight $ runParser expandListItem "inline" "\"\\x27\""
      res `shouldBe` "\"\\\"\\\\x27\\\"\""
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D08.txt"
      parsedLines <- assertJust $ traverse (parseMaybe expandListItem) input
      let codeChars = foldMap (Sum . T.length) input
          encodedChars = foldMap (Sum . length) parsedLines
      encodedChars - codeChars `shouldBe` 2117
