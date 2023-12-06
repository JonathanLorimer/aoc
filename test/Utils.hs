module Utils where

import Test.HUnit
import Control.Monad (forM)
import Test.Hspec (shouldBe)
import Test.Hspec.Core.Spec (Expectation)

assertJust :: Maybe a -> IO a
assertJust mayb = 
  case mayb of
    Nothing -> assertFailure "Expected Maybe to be Just but got Nothing"
    Just a -> pure a

assertJustMsg :: String -> Maybe a -> IO a
assertJustMsg str mayb = 
  case mayb of
    Nothing -> assertFailure $ "Expected Maybe to be Just but got Nothing: " <> str
    Just a -> pure a

assertRight :: Show l => Either l r -> IO r
assertRight eith =
  case eith of
    Left l -> assertFailure $ "Expected Either to be Right but got Left: " <> show l
    Right a -> pure a

allShouldBe :: (Eq a, Show a) => [a] -> a -> Expectation
allShouldBe as a = forM as (`shouldBe` a) >> pure ()
