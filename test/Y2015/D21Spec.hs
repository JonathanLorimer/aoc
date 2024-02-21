module Y2015.D21Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Y2015.D21
import Data.List (sortOn)

spec :: Spec
spec = do
  describe "Day 21: RPG Simulator 20XX Pt. 1" $ do
    it "result" $ do
      let (cost, _) = head . sortOn fst $ winningKits heroKits
      cost `shouldBe` 121
  describe "Day 21: RPG Simulator 20XX Pt. 2" $ do
    it "result" $ do
      let (cost, _) = last . sortOn fst $ losingKits heroKits
      cost `shouldBe` 201
