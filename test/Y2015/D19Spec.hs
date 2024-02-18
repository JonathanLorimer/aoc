module Y2015.D19Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Y2015.D19
import Utils
import Data.Containers.ListUtils (nubOrd)

spec :: Spec
spec = do
  describe "Day 19: Medicine for Rudolph Pt. 1" $ do
    it "result" $ do
      input <- readFile "./input/Y2015/D19.txt"
      MoleculeData{..} <- assertParse moleculeDataP input
      let calibrations = singleSubstitution replacements molecule
          uniqueCalibrations = nubOrd calibrations
          numUniqueCalibrations = length uniqueCalibrations
      numUniqueCalibrations `shouldBe` 535
    it "result" $ do
      input <- readFile "./input/Y2015/D19.txt"
      MoleculeData{..} <- assertParse moleculeDataP input
      let m = foldMap getElement molecule
      countSubstitutions (preProcess m) `shouldBe` 212 
