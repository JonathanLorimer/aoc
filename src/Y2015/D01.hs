{-# LANGUAGE TemplateHaskell #-}
module Y2015.D01 where

import Prelude hiding (floor)

import Data.Text

countParens :: Text -> Int
countParens = foldl' countParen 0
  where
    countParen :: Int -> Char -> Int
    countParen n '(' = n + 1
    countParen n ')' = n - 1
    countParen n _ = n

data FloorState = 
  FloorState 
    { position :: Int 
    , floor :: Int 
    , firstBasementPosition :: Maybe Int
    }

firstBasement' :: Text -> FloorState
firstBasement' = foldl' checkFloor (FloorState 1 0 Nothing)
  where
    checkFloor :: FloorState -> Char -> FloorState
    checkFloor fs '(' = fs { floor = floor fs + 1, position = position fs + 1 }
    checkFloor fs ')' = 
      let newFloor = floor fs - 1
       in if newFloor == -1 && firstBasementPosition fs == Nothing
            then 
              fs 
               { floor = newFloor
               , position = position fs + 1
               , firstBasementPosition = Just $ position fs
               }
            else 
              fs 
               { floor = newFloor
               , position = position fs + 1
               }
    checkFloor fs _ = fs

firstBasement :: Text -> Maybe Int
firstBasement = firstBasementPosition . firstBasement'
