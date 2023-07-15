module Y2015.D03 where

import Text.Megaparsec hiding (empty)
import Data.Void (Void)
import Data.Text (Text)
import Control.Applicative (asum)
import Text.Megaparsec.Char (char)
import Data.Set (Set, insert, singleton, empty)
import Data.Bifunctor
import Data.Foldable
import Data.Either (partitionEithers)

data Direction = North | South | East | West deriving Show

parseDirection :: Parsec Void Text Direction
parseDirection = asum
  [ char '^' >> pure North
  , char '>' >> pure East
  , char 'v' >> pure South
  , char '<' >> pure West
  ]

parseDirections :: Parsec Void Text [Direction]
parseDirections = many parseDirection

data Coordinates = 
  Coordinates 
    { position :: (Int, Int)
    , reverseHistory :: [(Int, Int)]
    } deriving Show

initialCoordinates :: Coordinates
initialCoordinates = Coordinates (0,0) ([(0,0)])

visitHouse :: Coordinates -> Direction -> Coordinates
visitHouse Coordinates{..} dir = 
  let newPosition = case dir of
                      North -> second (+ 1) position
                      South -> second (subtract 1) position
                      East -> first (+ 1) position
                      West -> first (subtract 1) position
  in Coordinates 
      { position = newPosition
      , reverseHistory = newPosition : reverseHistory 
      }

visitHouses :: [Direction] -> Coordinates    
visitHouses = foldl' visitHouse initialCoordinates

visitedHouses :: Coordinates -> Set (Int, Int)
visitedHouses = foldr insert empty . reverseHistory

splitCharWise :: String -> (String, String)
splitCharWise str = partitionEithers $ zipWith ($) (cycle [Left, Right]) str
