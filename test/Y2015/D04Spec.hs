module Y2015.D04Spec where

import Prelude hiding (readFile)

import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (strip)
import Y2015.D04 (findHashWithPrefix)

spec :: Spec
spec = do
  describe "The Ideal Stocking Stuffer pt.1" $ do
    it "If your secret key is abcdef, the answer is 609043" $ do
      pendingWith "hashing is too expensive to continually run in tests"
      fst (findHashWithPrefix "abcdef" "00000") `shouldBe` 609043
    it "result" $ do
      pendingWith "hashing is too expensive to continually run in tests"
      input <- strip <$> readFile "./input/Y2015/D04.txt"
      fst (findHashWithPrefix input "00000") `shouldBe` 254575

  describe "The Ideal Stocking Stuffer pt.2" $ do
    it "result" $ do
      pendingWith "hashing is too expensive to continually run in tests"
      input <- strip <$> readFile "./input/Y2015/D04.txt"
      fst (findHashWithPrefix input "000000") `shouldBe` 1038736
