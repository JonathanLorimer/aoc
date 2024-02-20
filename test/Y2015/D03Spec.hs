module Y2015.D03Spec where

import Prelude hiding (readFile)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text qualified as T
import Data.Set (size, union)
import Y2015.D03
import Text.Megaparsec (parseMaybe)
import TestUtils (assertJust)
import Data.Text (strip)
import Data.Bifunctor

spec :: Spec
spec = do
  describe "Perfectly Spherical Houses in a Vacuum pt.1" $ do
    it "> delivers presents to 2 houses" $ do
      directions <- assertJust $ parseMaybe parseDirections ">"
      (size . visitedHouses . visitHouses) directions `shouldBe` 2
    it "^>v< delivers presents to 4 houses" $ do
      directions <- assertJust $ parseMaybe parseDirections "^>v<"
      (size . visitedHouses . visitHouses) directions `shouldBe` 4
    it "^v^v^v^v^v delivers presents to 2 houses" $ do
      directions <- assertJust $ parseMaybe parseDirections "^v^v^v^v^v"
      (size . visitedHouses . visitHouses) directions `shouldBe` 2
    it "result" $ do
      input <- strip <$> readFile "./input/Y2015/D03.txt"
      directions <- assertJust $ parseMaybe parseDirections input
      (size . visitedHouses . visitHouses) directions `shouldBe` 2572

  describe "Perfectly Spherical Houses in a Vacuum pt.2" $ do
    it "^v delivers presents to 3 houses" $ do
      let (santaStr, roboStr) = splitCharWise "^v"
      santaDir <- assertJust . parseMaybe parseDirections $ T.pack santaStr
      roboDir <- assertJust . parseMaybe parseDirections $ T.pack roboStr

      let santaHouses = visitedHouses . visitHouses $ santaDir
          roboHouses = visitedHouses . visitHouses $ roboDir
      size (santaHouses `union` roboHouses) `shouldBe` 3

    it "^>v< delivers presents to 3 houses" $ do
      let (santaStr, roboStr) = splitCharWise "^>v<"
      santaDir <- assertJust . parseMaybe parseDirections $ T.pack santaStr
      roboDir <- assertJust . parseMaybe parseDirections $ T.pack roboStr

      let santaHouses = visitedHouses . visitHouses $ santaDir
          roboHouses = visitedHouses . visitHouses $ roboDir
      size (santaHouses `union` roboHouses) `shouldBe` 3

    it "^v^v^v^v^v now delivers presents to 11 houses" $ do
      let (santaStr, roboStr) = splitCharWise "^v^v^v^v^v"
      santaDir <- assertJust . parseMaybe parseDirections $ T.pack santaStr
      roboDir <- assertJust . parseMaybe parseDirections $ T.pack roboStr

      let santaHouses = visitedHouses . visitHouses $ santaDir
          roboHouses = visitedHouses . visitHouses $ roboDir
      size (santaHouses `union` roboHouses) `shouldBe` 11

    it "result" $ do
      (santaTxt, roboTxt) <- (bimap T.pack T.pack) . splitCharWise . T.unpack . strip <$> readFile "./input/Y2015/D03.txt"
      santaDir <- assertJust . parseMaybe parseDirections $ santaTxt
      roboDir <- assertJust . parseMaybe parseDirections $ roboTxt

      let santaHouses = visitedHouses . visitHouses $ santaDir
          roboHouses = visitedHouses . visitHouses $ roboDir
      size (santaHouses `union` roboHouses) `shouldBe` 2631

