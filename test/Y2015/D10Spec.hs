module Y2015.D10Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Y2015.D10

spec :: Spec
spec = do
  describe "Elves Look, Elves Say pt.1" $ do
    it "1 becomes 11" $ do
      lookAndSay [1] `shouldBe` [1, 1]
    it "11 becomes 21" $ do
      lookAndSay [1, 1] `shouldBe` [2, 1]
    it "21 becomes 1211" $ do
      lookAndSay [2, 1] `shouldBe` [1, 2, 1, 1]
    it "1211 becomes 111221" $ do
      lookAndSay [1, 2, 1, 1] `shouldBe` [1, 1, 1, 2, 2, 1]
    it "111221 becomes 312211" $ do
      lookAndSay [1, 1, 1, 2, 2, 1] `shouldBe` [3, 1, 2, 2, 1, 1]

    it "result" $ do
      let input :: [Int] = [1, 1, 1, 3, 2, 2, 2, 1, 1, 3]
      let result = length . last $ doNTimes input 40 lookAndSay
      result `shouldBe` 252594
      
  describe "Elves Look, Elves Say pt.2" $ do
    it "result" $ do
      let input :: [Int] = [1, 1, 1, 3, 2, 2, 2, 1, 1, 3]
      let result = length . last $ doNTimes input 50 lookAndSay
      result `shouldBe` 3579328
