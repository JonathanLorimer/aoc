module Y2015.D02Spec where

import Prelude hiding (readFile)

import Test.Hspec
import Y2015.D02
import Data.Text.IO (readFile)
import Data.Text qualified as T
import Text.Megaparsec (parseMaybe)
import Data.Monoid
import Utils (assertJust)

spec :: Spec
spec = do
  describe "I Was Told There Would Be No Math pt.1" $ do
    it "A present with dimensions 2x3x4 requires 58" $ do
     paperRequired (RectPrism 2 3 4) `shouldBe` 58
    it "A present with dimensions 1x1x10 requires 43" $ do
     paperRequired (RectPrism 1 1 10) `shouldBe` 43
    it "result" $ do
      input <- T.lines <$> readFile "./input/Y2015/D02.txt"
      let parsedInput = parseMaybe parseRectPrism <$> input
      case sequence parsedInput of
        Nothing -> expectationFailure "Could not parse gift dimensions from input"
        Just rects -> foldMap (Sum . paperRequired) rects `shouldBe` Sum 1598415

  describe "I Was Told There Would Be No Math pt.2" $ do
    it "A present with dimensions 2x3x4 requires 34" $ do
     ribbonRequired (RectPrism 2 3 4) `shouldBe` 34
    it "A present with dimensions 1x1x10 requires 43" $ do
     ribbonRequired (RectPrism 1 1 10) `shouldBe` 14
    it "result" $ do
      input <- T.lines <$> readFile "./input/Y2015/D02.txt"
      rects <- assertJust . sequence $ parseMaybe parseRectPrism <$> input
      foldMap (Sum . ribbonRequired) rects `shouldBe` Sum 3812909
