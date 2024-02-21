module Utils where

import Data.Map.Strict ( Map, alter )
import Data.Maybe
import GHC.Natural

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

upsert :: forall a b k. Ord k => (b -> a) -> (b -> a -> a) -> k -> b -> Map k a -> Map k a 
upsert transform combine k v = alter (Just . go) k
  where
    go :: Maybe a -> a
    go Nothing = transform v
    go (Just a) = v `combine` a
    
(-.-) :: Natural -> Natural -> Natural
n -.- m = fromMaybe 0 $ minusNaturalMaybe n m
