module Property where

import Test.Hspec
import Test.Hspec.Hedgehog (modifyMaxSuccess)
import Hedgehog (MonadTest, (===))
import Control.Monad (forM_)

runs :: Int -> SpecWith a -> SpecWith a
runs i = modifyMaxSuccess (const i)

allEq :: (MonadTest m, Eq a, Show a) => [a] -> a -> m ()
allEq as a = forM_ as (=== a)
