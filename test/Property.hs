module Property where

import Test.Hspec
import Test.Hspec.Hedgehog (modifyMaxSuccess)

runs :: Int -> SpecWith a -> SpecWith a
runs i = modifyMaxSuccess (const i)
