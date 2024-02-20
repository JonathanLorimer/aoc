module Y2015.D16Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D16
import Text.Megaparsec
import TestUtils
import Control.Monad (void)

spec :: Spec
spec = do
  describe "Aunt Sue pt.1" $ do
    it "parser" $ do
      input <- lines <$> readFile "./input/Y2015/D16.txt"
      void $ assertJust $ traverse (parseMaybe sueP) input
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D16.txt"
      sues <- assertJust $ traverse (parseMaybe sueP) input
      let matches = findMatches giftCompounds sues 
          positives = filterMismatches matches
          maxCompounds = findMax positives
      number maxCompounds `shouldBe` 213
  describe "Aunt Sue pt.2" $ do
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D16.txt"
      sues <- assertJust $ traverse (parseMaybe sueP) input
      let matches = findMatches' giftCompounds sues 
          positives = filterMismatches matches
          maxCompounds = findMax positives
      number maxCompounds `shouldBe` 323
