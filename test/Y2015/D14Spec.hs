module Y2015.D14Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Utils (assertJust)
import Text.Megaparsec (parseMaybe)
import Y2015.D14
import Numeric.Natural (Natural)
import Data.Monoid
import qualified Data.Map.Strict as M

raceTime :: Natural
raceTime = 2503

spec :: Spec
spec = do
  describe "Reindeer Olympics pt.1" $ do
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D14.txt"
      rs <- assertJust $ traverse (parseMaybe reindeer) input
      let scores = raceReindeer raceTime rs
      snd (last scores) `shouldBe` 2696
      
  describe "Reindeer Olympics pt.2" $ do
    it "example" $ do
      input <- lines <$> readFile "./input/Y2015/D14Example.txt"
      rs <- assertJust $ traverse (parseMaybe reindeer) input
      let scores = newScoringRaceReindeer 1000 $ rs
      (snd . last . findWinner $ scores) `shouldBe` 689
      (snd . head . findWinner $ scores) `shouldBe` 312

    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D14.txt"
      rs <- assertJust $ traverse (parseMaybe reindeer) input
      let scores = newScoringRaceReindeer raceTime $ rs
      print $ foldMap (Sum . snd) (M.toList scores)
      (snd . last . findWinner $ scores) `shouldBe` 1084
