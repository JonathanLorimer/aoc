module Y2015.D13Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Utils (assertJust)
import Text.Megaparsec (parseMaybe)
import Y2015.D13
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "parser" $ do
    it "Alice would gain 54 happiness units by sitting next to Bob." $ do
      arng <- assertJust $ parseMaybe arrangement "Alice would gain 54 happiness units by sitting next to Bob." 
      arng `shouldBe` (Arrangement "Alice" "Bob" 54)
    it "Alice would lose 79 happiness units by sitting next to Carol." $ do
      arng <- assertJust $ parseMaybe arrangement "Alice would lose 79 happiness units by sitting next to Carol." 
      arng `shouldBe` (Arrangement "Alice" "Carol" (-79))
    it "Alice would lose 2 happiness units by sitting next to David." $ do
      arng <- assertJust $ parseMaybe arrangement "Alice would lose 2 happiness units by sitting next to David." 
      arng `shouldBe` (Arrangement "Alice" "David" (-2))

  describe "Knights of the Dinner Table pt.1" $ do
    it "Should work on minimal example" $ do
      input <- lines <$> readFile "./input/Y2015/D13Example.txt"
      as <- assertJust $ traverse (parseMaybe arrangement) input
      let ra = rankedArrangements as
      fst (last ra) `shouldBe` 330

    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D13.txt"
      as <- assertJust $ traverse (parseMaybe arrangement) input
      let ra = rankedArrangements as
      fst (last ra) `shouldBe` 709
      
  describe "Knights of the Dinner Table pt.2" $ do
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D13.txt"
      as' <- assertJust $ traverse (parseMaybe arrangement) input
      let as = as' <> foldMap (\t -> [Arrangement "Jonathan" t 0, Arrangement t "Jonathan" 0]) (S.toList (allSitters as'))
      let ra = rankedArrangements as
      fst (last ra) `shouldBe` 668
