module Y2015.D20 where

import Math.NumberTheory.ArithmeticFunctions
import Control.Monad
import Data.Functor

housePresentList :: (Int -> Int) -> [(Int, Int)]
housePresentList f = [1..] <&> \i -> (i, f i) 

allDivisors :: Int -> Int
allDivisors = (* 10) . sum . divisorsList

only50 :: Int -> Int
only50 n = sum do
  -- t represents an elf delivering presents
  t <- divisorsList n
  -- elfs only deliver to 50 houses, so if n (the current house number)
  -- is greater than 50 * the elfs house then we need to skip it
  guard $ n <= 50 * t
  pure (t * 11)
