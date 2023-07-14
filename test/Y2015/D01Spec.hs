module Y2015.D01Spec where

import Prelude hiding (readFile)

import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)
import Test.Hspec.Hedgehog hiding (annotate)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Y2015.D01 (countParens, firstBasement)
import Data.List (permutations)
import qualified Data.Text as T
import Data.Text.IO
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "Not Quite Lisp pt.1" $ do
    it "(()) and ()() both result in floor 0." $ do
      annotate "(())" $ countParens "(())" `shouldBe` 0
      annotate "()()" $ countParens "()()" `shouldBe` 0
    it "((( and (()(()( both result in floor 3." $ do
      annotate "(((" $ countParens "(((" `shouldBe` 3
      annotate "(()(()(" $ countParens "(()(()(" `shouldBe` 3
    it "))((((( also results in floor 3." $ do
      countParens "))(((((" `shouldBe` 3
    it "()) and ))( both result in floor -1 (the first basement level)." $ do
      annotate "())" $ countParens "())" `shouldBe` -1
      annotate "))(" $ countParens "))(" `shouldBe` -1 
    it "))) and )())()) both result in floor -3." $ do
      annotate ")))" $ countParens ")))" `shouldBe` -3
      annotate ")())())" $ countParens ")())())" `shouldBe` -3
    it "All paren permutations are equal" $
       hedgehog $ do
        parens <- forAll $ Gen.string (Range.linear 0 5) genParen
        let perms = T.pack <$> permutations parens
        let reference = countParens $ T.pack parens
        void $ forM perms $ \perm -> reference === countParens perm
    it "result" $ do
      input <- readFile "./input/Y2015/D01.txt"
      countParens input `shouldBe` 232

  describe "Not Quite Lisp pt.2" $ do
    it ") causes him to enter the basement at character position 1." $ do
      firstBasement ")" `shouldBe` Just 1 
    it "()()) causes him to enter the basement at character position 5." $ do
      firstBasement "()())" `shouldBe` Just 5
    it "result" $ do
      input <- readFile "./input/Y2015/D01.txt"
      firstBasement input `shouldBe` Just 1783
      

genParen :: Gen Char
genParen = Gen.choice [pure '(', pure ')']
