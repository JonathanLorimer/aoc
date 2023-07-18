module Y2015.D09Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Utils (assertJust)
import Text.Megaparsec (parseMaybe)
import Y2015.D09

spec :: Spec
spec = do
  describe "parser" $ do
    it "Should parse minimal exapmle" $ do
     p <- assertJust $ parseMaybe parsePath "London to Dublin = 464"
     p `shouldBe` Path "London" "Dublin" 464

  describe "All in a Single Night pt.1" $ do
    it "Should work on minimal example" $ do
      let rs = routes 
                [ Path "London" "Dublin" 464
                , Path "London" "Belfast" 518
                , Path "Dublin" "Belfast" 141
                ]
          (d, _) = head $ rankedRoutes rs
      d `shouldBe` 605

    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D09.txt"
      paths <- assertJust $ traverse (parseMaybe parsePath) input
      let rs = routes paths
      let (d, _) = head $ rankedRoutes rs
      d `shouldBe` 251
      
      
  describe "All in a Single Night pt.2" $ do
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D09.txt"
      paths <- assertJust $ traverse (parseMaybe parsePath) input
      let rs = routes paths
      let (d, _) = last $ rankedRoutes rs
      d `shouldBe` 898
