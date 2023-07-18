module Y2015.D11 where

import Data.Vector (Vector)
import Text.Megaparsec (Parsec, many)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char (letterChar)
import Data.Monoid
import Data.List

data Alpha = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Ord, Bounded, Enum, Show)

type Password = Vector Alpha

maySucc :: (Bounded a, Eq a, Enum a) => a -> Maybe a
maySucc x = if x == maxBound then Nothing else Just $ succ x

parseAlphaChar :: Parsec Void Text Alpha
parseAlphaChar = toEnum . subtract 97 . fromEnum <$> letterChar

parseAlphaString :: Parsec Void Text [Alpha]
parseAlphaString = many parseAlphaChar

increment :: [Alpha] -> [Alpha]
increment = snd . foldr go (True, [])
  where 
    go :: Alpha -> (Bool, [Alpha]) -> (Bool, [Alpha])
    go x (shouldIncr, acc) 
      | not shouldIncr = (shouldIncr, x : acc)
      | otherwise = 
          case maySucc x of
            Nothing -> (True, minBound : acc)
            Just a -> (False, a : acc)

hasAny :: forall a. Eq a => [a] -> [a] -> Bool
hasAny [] _          = False
hasAny _ []          = False
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

checkStraight :: [Alpha] -> Bool
checkStraight = getAny . foldMap (Any . (>= 3) . length) . group . zipWith subtract [0..] . fmap fromEnum

checkNonOverlappingPairs :: [Alpha] -> Bool
checkNonOverlappingPairs = (>= 2) . length . filter ((>= 2) . length) . group

isValidPassword :: [Alpha] -> Bool
isValidPassword as = getAll $ foldMap All
  [(not (hasAny [I, O, L] as))
  , (checkStraight as)
  , (checkNonOverlappingPairs as)
  ]

findNextValidPassword :: [Alpha] -> [Alpha]
findNextValidPassword = until isValidPassword increment
