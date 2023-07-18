module Y2015.D05 where


import Data.Text (Text)
import Data.Text qualified as T 
import Data.Monoid
import qualified Data.Set as Set
import Data.List (tails, inits)
import Control.Monad ((<=<))



-- It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
-- It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
-- It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

vowels :: Text -> Text
vowels = T.filter (`elem` ['a', 'e', 'i', 'o', 'u'])

containsSubsequences :: Text -> [Text] -> Bool
containsSubsequences t = getAny . foldMap (Any . (`T.isInfixOf` t)) 

isNiceWord :: Text -> Bool
isNiceWord t = 
  getAll $ foldMap All
  [ (T.length . vowels) t >= 3 
  , not . null $ filter ((>= 2) . T.length) (T.group t)
  , not $ containsSubsequences t ["ab", "cd", "pq", "xy"]
  ]

nLengthSubstrings :: Eq a => Int -> [a] -> [[a]]
nLengthSubstrings n = filter ((n ==) . length) . (tail . inits <=< tails)

removeAdjacentDuplicates :: Eq a => [a] -> [a]
removeAdjacentDuplicates xs = go xs []
  where
    go :: Eq a => [a] -> [a] -> [a]
    go [] acc = acc
    go (x:[]) acc = x:acc
    go (a:b:rest) acc = go (if a == b then rest else b:rest) (a:acc)

winFoldl :: ([a] -> b -> b) -> b -> Int -> [a] -> b
winFoldl _ acc _ [] = acc
winFoldl f acc n t = winFoldl f (f (take n t) acc) n (tail t) 

isNiceWord2 :: Text -> Bool
isNiceWord2 text =
  let str = T.unpack text
      subTuples =  removeAdjacentDuplicates $ nLengthSubstrings 2 str
      rule1 = length subTuples > (Set.size . Set.fromList) subTuples
      rule2 = not . null $ winFoldl (\t b -> if length t == 3 && head t == last t then t : b else b) [] 3 str
   in rule1 && rule2
   
