module Y2015.D18Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D18 
import Text.Megaparsec (parseMaybe)
import Utils
import Control.Monad
import Numeric.Natural (Natural)
import Data.Map.Strict qualified as M

spec :: Spec
spec = do
  describe "Like a GIF For Your Yard pt. 1" $ do
    it "parser" $ do
      input <- lines <$> readFile "./input/Y2015/D18.txt"
      void $ assertJust $ traverse (parseMaybe lineP) input
    describe "adjacentCells" $ do
      it "should work for unbounded cells" $ do
        let size = 2
            cubeArea = ((size * 2) + 1) ^ (2 :: Natural)
        length (adjacentCells (Cell 100 100) size (Cell 50 50))
          `shouldBe` fromIntegral cubeArea
      it "should work for cells bounded at the top" $ do
        let size = 2
            cubeArea = ((size * 2) + 1) ^ (2 :: Natural)
        length (adjacentCells (Cell 100 100) size (Cell 99 50))
          `shouldBe` fromIntegral (cubeArea - ((size * 2) + 1))
      it "should work for cells bounded at the bottom" $ do
        let size = 2
            cubeArea = ((size * 2) + 1) ^ (2 :: Natural)
        length (adjacentCells (Cell 100 100) size (Cell 1 50))
          `shouldBe` fromIntegral (cubeArea - ((size * 2) + 1))
      it "should work for cells bounded at the left" $ do
        let size = 2
            cubeArea = ((size * 2) + 1) ^ (2 :: Natural)
        length (adjacentCells (Cell 100 100) size (Cell 50 1))
          `shouldBe` fromIntegral (cubeArea - ((size * 2) + 1))
      it "should work for cells bounded at the right" $ do
        let size = 2
            cubeArea = ((size * 2) + 1) ^ (2 :: Natural)
        length (adjacentCells (Cell 100 100) size (Cell 50 99))
          `shouldBe` fromIntegral (cubeArea - ((size * 2) + 1))
      it "should work for cells bounded at the top right" $ do
        let size = 2
        length (adjacentCells (Cell 100 100) size (Cell 99 99))
          `shouldBe` fromIntegral ((size * 2) ^ (2 :: Natural))
      it "should work for a box that touches the boundaries" $ do
        let size = 2
            cubeArea = ((size * 2) + 1) ^ (2 :: Natural)
        length (adjacentCells (Cell 100 100) size (Cell 98 98))
          `shouldBe` fromIntegral cubeArea
      
    it "result" $ do
      -- pendingWith "Quadratic search space takes too long"
      input <- lines <$> readFile "./input/Y2015/D18.txt"
      parsedLines <- assertJust $ traverse (parseMaybe lineP) input
      let lightMap = mkLightMap parsedLines
          boundaryCell = getBoundaryCell lightMap
          transforms = iterate (step boundaryCell) lightMap
          transform100 = transforms !! 100
      countOns (M.elems transform100)
        `shouldBe` 821
  describe "Like a GIF For Your Yard pt. 2" $ do
    it "result" $ do
      -- pendingWith "Quadratic search space takes too long"
      input <- lines <$> readFile "./input/Y2015/D18.txt"
      parsedLines <- assertJust $ traverse (parseMaybe lineP) input
      let lightMap = mkLightMap parsedLines
          boundaryCell = getBoundaryCell lightMap
          corners = 
            [ Cell 0 0
            , Cell 0 (rowNumber boundaryCell)
            , Cell (columnNumber boundaryCell) 0
            , boundaryCell
            ]
          lightMap' = foldr setCellOn lightMap corners
          transforms = iterate (step' boundaryCell) lightMap'
          transform100 = transforms !! 100
      countOns (M.elems transform100)
        `shouldBe` 886
