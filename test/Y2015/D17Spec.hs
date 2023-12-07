module Y2015.D17Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import qualified Data.Text as T
import Y2015.D17 

spec :: Spec
spec = do
  describe "No Such Thing as Too Much pt.1" $ do
    it "parser" $ do
      input <- lines <$> readFile "./input/Y2015/D17.txt"
      let containers = read . T.unpack <$> input
          combos = getCombos 150 containers
      combos `shouldBe` 4372 
  describe "No Such Thing as Too Much pt.2" $ do
    it "parser" $ do
      input <- lines <$> readFile "./input/Y2015/D17.txt"
      let containers = read . T.unpack <$> input
          combos = getCombos' 150 containers
      combos `shouldBe` 4 
