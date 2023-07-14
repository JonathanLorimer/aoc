module Y2015.D03 where

import Text.Megaparsec hiding (empty)
import Data.Void (Void)
import Data.Text (Text)
import Control.Applicative (asum)
import Text.Megaparsec.Char (char)
import Data.Set (Set, insert, singleton, empty)
import Data.Bifunctor
import Data.Foldable

-- He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via 
-- radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), or west 
-- (<). After each move, he delivers another present to the house at his new location.
--
-- However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and 
-- Santa ends up visiting some houses more than once. How many houses receive at least one present?

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


