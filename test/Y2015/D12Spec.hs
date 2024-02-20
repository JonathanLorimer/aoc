module Y2015.D12Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Y2015.D12 
import Data.ByteString as B
import TestUtils (assertJust)
import Data.Aeson (decodeStrict)

spec :: Spec
spec = do
  describe "JSAbacusFramework.io pt.1" $ do
    it "result" $ do
      input <- B.readFile "./input/Y2015/D12.txt"
      value <- assertJust $ decodeStrict input
      sumJSONNumbers value `shouldBe` 156366.0
      
  describe "JSAbacusFramework.io pt.2" $ do
    it "result" $ do
      input <- B.readFile "./input/Y2015/D12.txt"
      value <- assertJust $ decodeStrict input
      sumJSONNumbersIgnoreRed value `shouldBe` 96852.0
      
