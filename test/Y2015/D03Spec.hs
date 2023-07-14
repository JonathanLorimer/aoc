module Y2015.D03Spec where

import Prelude hiding (readFile)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text qualified as T
import Data.Set (size)
import Y2015.D03
import Text.Megaparsec (parseMaybe)
import Utils (assertJust)
import Data.Text (strip)

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
    it "" $ do
      input <- strip <$> readFile "./input/Y2015/D03.txt"
      directions <- assertJust $ parseMaybe parseDirections input
      (size . visitedHouses . visitHouses) directions `shouldBe` 0

  describe "Perfectly Spherical Houses in a Vacuum pt.2" $ do
    it "" $ do
     pending
    it "" $ do
      _ <- T.lines <$> readFile "./input/Y2015/D02.txt"
      pending

