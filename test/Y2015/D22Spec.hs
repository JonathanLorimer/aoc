module Y2015.D22Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Y2015.D22
import Control.Monad.State (runState)

spec :: Spec
spec = do
  describe "Day 22: Wizard Simulator 20XX Pt. 1" $ do
    it "result" $ do
      let (_forest, mManaCost) = runState (gameForest EasyMode) Nothing
      mManaCost `shouldBe` Just 953
  describe "Day 22: Wizard Simulator 20XX Pt. 2" $ do
    it "result" $ do
      let (_forest, mManaCost) = runState (gameForest HardMode) Nothing
      mManaCost `shouldBe` Just 1289
