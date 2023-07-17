module Y2015.D06 where

import Numeric.Natural
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Text.Megaparsec.Char (string, digitChar, char, space)
import Control.Applicative
import Control.Monad (void)
import Data.Map.Strict (Map, adjust)
import Data.Maybe

data Action = TurnOn | TurnOff | Toggle 

type Point = (Natural, Natural)

data Rect = Rect Point Point

-- Parser
action :: Parsec Void Text Action
action = asum 
  [ string "turn on" >> pure TurnOn
  , string "turn off" >> pure TurnOff
  , string "toggle" >> pure Toggle
  ]

point :: Parsec Void Text Point
point = do
  x <- read <$> many digitChar
  void $ char ','
  y <- read <$> many digitChar
  pure (x,y)

parseLine :: Parsec Void Text (Action, Rect)
parseLine = do
  act <- action
  void $ space
  p1 <- point
  void $ space
  void $ string "through"
  void $ space
  p2 <- point
  pure (act, Rect p1 p2)

invertSet :: Ord a => Set a -> Set a -> Set a
invertSet result operand  = 
  let toAdd = S.difference operand result
   in result `S.difference` operand `S.union` toAdd

rectArea :: Rect -> Set Point
rectArea = S.fromList . rectAreaList 

runAction :: Set Point -> (Action, Rect) -> Set Point
runAction s (a,r) = 
  let area = rectArea r
  in case a of
      TurnOn -> s `S.union` area
      TurnOff -> s `S.difference` area
      Toggle -> s `invertSet` area

--- part 2

rectAreaList :: Rect -> [Point]
rectAreaList (Rect (x,y) (x',y')) = do
  x <- [x..x']
  y <- [y..y']
  pure (x,y)

runActionMap :: Map Point Natural -> (Action, Rect) -> Map Point Natural
runActionMap m (a,r) = 
  let area = rectAreaList r
  in case a of
      TurnOn -> foldr (adjust (+1)) m area
      TurnOff -> foldr (adjust (fromMaybe 0 . (`minusNaturalMaybe` 1))) m area
      Toggle -> foldr (adjust (+2)) m area
