module Y2015.D17 where
import GHC.Natural (Natural)
import Data.List
import Control.Arrow

getCombos :: Natural -> [Natural] -> Natural
getCombos total = fromIntegral . length . filter (== fromIntegral total) . fmap sum . subsequences

getCombos' :: Natural -> [Natural] -> Natural
getCombos' total ns = let 
    subs = subsequences ns
    subsWithSum :: [([Natural], Natural)] = fmap (id &&& sum) subs
    sizedSubs = fmap fst $ filter ((==) (fromIntegral total) . snd) subsWithSum
    smallestSub = minimum $ length <$> sizedSubs
    smallestSubs = filter ((==) (fromIntegral smallestSub) . length) sizedSubs
  in fromIntegral $ length smallestSubs
