module Y2015.D05Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D05 

spec :: Spec
spec = do
  describe "Doesn't He Have Intern-Elves For This? pt.1" $ do
    it "ugknbfddgicrmopn is nice" $ do
      isNiceWord "ugknbfddgicrmopn" `shouldBe` True
    it "aaa is nice" $ do
      isNiceWord "aaa" `shouldBe` True
    it "jchzalrnumimnmhp is not nice" $ do
      isNiceWord "jchzalrnumimnmhp" `shouldBe` False
    it "jchzalrnumimnmhp is not nice" $ do
      isNiceWord "jchzalrnumimnmhp" `shouldBe` False
    it "haegwjzuvuyypxyu is not nice" $ do
      isNiceWord "haegwjzuvuyypxyu" `shouldBe` False
    it "dvszwmarrgswjxmb is not nice" $ do
      isNiceWord "dvszwmarrgswjxmb" `shouldBe` False
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D05.txt"
      (length . filter id . fmap isNiceWord) input `shouldBe` 258

  describe "Doesn't He Have Intern-Elves For This? pt.2" $ do
    it "qjhvhtzxzqqjkmpb is nice" $ do
      isNiceWord2 "qjhvhtzxzqqjkmpb" `shouldBe` True
    it "xxyxx is nice" $ do
      isNiceWord2 "xxyxx" `shouldBe` True
    it "uurcxstgmygtbstg is not nice" $ do
      isNiceWord2 "uurcxstgmygtbstg" `shouldBe` False
    it "ieodomkazucvgmuy is not nice" $ do
      isNiceWord2 "ieodomkazucvgmuy" `shouldBe` False
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D05.txt"
      (length . filter id . fmap isNiceWord2) input `shouldBe` 53
