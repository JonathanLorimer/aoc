module Y2015.D10 where

import Data.List (group, unfoldr)
import Data.Foldable
import Numeric.Natural

lookAndSay :: [Int] -> [Int]
lookAndSay = fold . (fmap $ \is -> [length is, head is]) . group

doNTimes :: forall a . a -> Natural -> (a -> a) -> [a]
doNTimes start n step  = unfoldr stepFn (start, n + 1)
  where
    stepFn :: (a, Natural) -> Maybe (a, (a, Natural))    
    stepFn (_, 0) = Nothing
    stepFn (a, nth) = Just (a, (step a, nth - 1))
