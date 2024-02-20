module Y2015.D15Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import TestUtils (assertJust)
import Text.Megaparsec (parseMaybe)
import Y2015.D15
import Control.Monad (void)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog
import Hedgehog.Range
import Property
import Hedgehog.Gen qualified as Gen
import GHC.Natural
import Data.Semigroup
import Data.Map.Strict qualified as M

spec :: Spec
spec = do
  describe "Science for Hungry People pt.1" $ do
    it "parser" $ do
      input <- lines <$> readFile "./input/Y2015/D15.txt"
      void $ assertJust $ traverse (parseMaybe ingredientP) input
    describe "distributions" $ do
      it "all permutations sum to the limit" $ hedgehog $ do
        n <- forAll $ wordToNatural <$> Gen.word (linear 1 5)
        lim <- forAll $ wordToNatural <$> Gen.word (linear 0 20)
        let dist = distributions lim n
        let nums = getSum . foldMap Sum <$> dist
        nums `allEq` lim
      it "generates all permutations" $ hedgehog $ do
        n <- forAll $ wordToNatural <$> Gen.word (linear 1 5)
        lim <- forAll $ wordToNatural <$> Gen.word (linear 0 20)
        length (distributions lim n) === fromIntegral (countDistributions lim n)

    it "example" $ do
      butterscotch <- assertJust $ parseMaybe ingredientP "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
      cinnamon <- assertJust $ parseMaybe ingredientP "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
      let dists = [(rmCalories butterscotch, 44)
                  ,(rmCalories cinnamon, 56)
                  ]
          tp = totalProperties <$> dists
          si = sumIngredients tp
          cs = calculateCookieScore . M.elems $ si
      cs `shouldBe` 62842880
      
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D15.txt"
      ingredients <- assertJust $ traverse (parseMaybe ingredientP) input
      let scores = ingredientsToCookieScore <$> ingredientDistributions ingredients
      foldMap Max scores `shouldBe` Max 21367368
      
  describe "Science for Hungry People pt.2" $ do
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D15.txt"
      ingredients <- assertJust $ traverse (parseMaybe ingredientP) input
      let dists = ingredientDistributions ingredients
          cal500Recipes = filterForCalories (== 500) dists
          scores = ingredientsToCookieScore <$> cal500Recipes
      foldMap Max scores `shouldBe` Max 1766400
