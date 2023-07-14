module Utils where

import Test.HUnit

assertJust :: Maybe a -> IO a
assertJust mayb = 
  case mayb of
    Nothing -> assertFailure "Expected Maybe to be Just but got Nothing"
    Just a -> pure a
