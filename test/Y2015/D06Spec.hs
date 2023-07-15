module Y2015.D06Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D06 
import qualified Data.Set as S
import Text.Megaparsec (parseMaybe)
import Utils (assertJust)
import Data.List (foldl')
import Data.Monoid
import Data.Map.Strict qualified as MS

spec :: Spec
spec = do
  describe "Helpers" $ do
    describe "rectArea" $ do
      it "Rect 0,0 2,2" do
        rectArea (Rect (0,0) (2,2))
          `shouldBe` 
            (S.fromList 
              [ (0,0), (1,0), (2,0)
              , (0,1), (1,1), (2,1)
              , (0,2), (1,2), (2,2)
              ])
    describe "invertSet" $ do
      it "Should add non-existent elements" do
        S.empty `invertSet` (S.fromList [0 :: Int])
          `shouldBe` 
            (S.fromList [0])
      it "Should remove existing elements" do
        (S.fromList [0 :: Int]) `invertSet` (S.fromList [0])
          `shouldBe` S.empty
      it "Should remove and add elements" do
        (S.fromList [0 :: Int, 1]) `invertSet` (S.fromList [0])
          `shouldBe` (S.fromList [1])

  describe "Probably a Fire Hazard pt.1" $ do
    it "turn on 0,0 through 999,999 would turn on (or leave on) every light." $ do
      res <- assertJust $ parseMaybe parseLine "turn on 0,0 through 999,999"
      S.size (runAction S.empty res) `shouldBe` 1000 * 1000
    it "result" $ do
      input' <- lines <$> readFile "./input/Y2015/D06.txt"
      input <- assertJust $ traverse (parseMaybe parseLine) input'
      S.size (foldl' runAction S.empty input) `shouldBe` 377891
      
  describe "Probably a Fire Hazard pt.2" $ do
    it "result" $ do
      input' <- lines <$> readFile "./input/Y2015/D06.txt"
      input <- assertJust $ traverse (parseMaybe parseLine) input'
      let initialMap = MS.fromList 
                     . fmap (,0) 
                     . rectAreaList 
                     $ Rect (0,0) (999,999)
      let result = foldMap (Sum . snd) 
                 . MS.toList 
                 $ foldl' runActionMap initialMap input
      result `shouldBe` 14110788
