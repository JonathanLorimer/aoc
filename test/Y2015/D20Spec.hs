module Y2015.D20Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text qualified as T
import Y2015.D20
import TestUtils
import Data.List qualified as L

spec :: Spec
spec = do
  describe "Day 20: Infinite Elves and Infinite Houses Pt. 1" $ do
    it "result" $ do
      input :: Int <- read . T.unpack <$> readFile "./input/Y2015/D20.txt"
      (i, _) <- assertJust $ L.find (\pres -> input <= snd pres) $ housePresentList allDivisors
      i `shouldBe` 831600
    it "result" $ do
      input :: Int <- read . T.unpack <$> readFile "./input/Y2015/D20.txt"
      (i, _) <- assertJust $ L.find (\pres -> input <= snd pres) $ housePresentList only50
      i `shouldBe` 831600
