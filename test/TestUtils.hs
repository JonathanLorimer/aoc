module TestUtils where

import Test.HUnit
import Control.Monad (forM_)
import Test.Hspec (shouldBe)
import Test.Hspec.Core.Spec (Expectation)
import Text.Megaparsec

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
allShouldBe as a = forM_ as (`shouldBe` a)

assertParse ::
  ( ShowErrorComponent e,
    Show a,
    VisualStream s,
    TraversableStream s
  ) =>
  -- | Parser to run
  Parsec e s a ->
  -- | Input for parser
  s ->
  IO a
assertParse p input =
  case parse p "" input of
    Left e -> do
      putStr (errorBundlePretty e)
      assertFailure "Failed to parse, see above for a more detailed error"
    Right x -> pure x
