module Y2015.D11Spec where

import Prelude hiding (readFile, lines)

import Test.Hspec
import Y2015.D11 
import Utils (assertJust)
import Text.Megaparsec (parseMaybe)
import Data.Char (toLower)
import Data.Foldable

showAlpha :: [Alpha] -> String
showAlpha = fmap toLower . fold . fmap show

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses alpha string" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxbxwxba"
      result `shouldBe` [H, X, B, X, W, X, B, A]

  describe "predicates" $ do
    it "hasAny" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxbxixba"
      hasAny [I, O, L] result `shouldBe` True
    it "checkStraight True" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxjklxba"
      checkStraight result `shouldBe` True
    it "checkStraight False" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxjkmxba"
      checkStraight result `shouldBe` False
    it "checkNonOverlappingPairs True" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxaamxbb"
      checkNonOverlappingPairs result `shouldBe` True
    it "checkNonOverlappingPairs False" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxaaamxb"
      checkNonOverlappingPairs  result `shouldBe` False
    it "hijklmmn meets first req, failse second" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hijklmmn"
      checkStraight result `shouldBe` True
      hasAny [I, O, L] result `shouldBe` True
    it "abbceffg meets third req, but fails second" $ do
      result <- assertJust $ parseMaybe parseAlphaString "abbceffg"
      checkNonOverlappingPairs result `shouldBe` True
      checkStraight result `shouldBe` False
    it "abbcegjk meets third req, but fails second" $ do
      result <- assertJust $ parseMaybe parseAlphaString "abbcegjk"
      checkNonOverlappingPairs result `shouldBe` False
    it "hxbxxyzy is invalid password" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxbxxyzy"
      isValidPassword result `shouldBe` False
    it "hxbxxyzx is invalid password" $ do
      result <- assertJust $ parseMaybe parseAlphaString "hxbxxyzx"
      isValidPassword result `shouldBe` False

  describe "Corporate Policy pt.1" $ do
    it "increment works on minimal example" $ do
      first <- assertJust $ parseMaybe parseAlphaString "xx"
      let second = increment first
      let third = increment second
      let fourth = increment third
      let fifth = increment fourth
      showAlpha first `shouldBe` "xx"
      showAlpha second `shouldBe` "xy"
      showAlpha third `shouldBe` "xz"
      showAlpha fourth `shouldBe` "ya"
      showAlpha fifth `shouldBe` "yb"
    it "result" $ do
      input <- assertJust $ parseMaybe parseAlphaString "hxbxwxba"
      showAlpha (findNextValidPassword input) `shouldBe` "hxbxxyzz"
      
  describe "Corporate Policy pt.2" $ do
    it "result" $ do
      pendingWith "Takes too long"
      input <- assertJust $ parseMaybe parseAlphaString "hxbxxyzz"
      showAlpha (findNextValidPassword (increment input)) `shouldBe` "hxcaabcc"


