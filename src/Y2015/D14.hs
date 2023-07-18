module Y2015.D14 where

import Text.Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char
import Numeric.Natural
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Control.Monad (void)
import Data.List (sortBy)
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (groupBy)

data Reindeer =
  Reindeer 
    { speed :: Natural
    , duration :: Natural
    , rest :: Natural
    , name :: Text
    } deriving (Show)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

-- Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
reindeerName :: Parsec Void Text Text
reindeerName = fmap T.pack . L.lexeme sp $ some letterChar

natural :: Parsec Void Text Natural
natural = fmap read . L.lexeme sp $ some digitChar

reindeer :: Parsec Void Text Reindeer
reindeer = do
  name <- reindeerName
  void . L.lexeme sp $ string "can fly"
  speed <- natural
  void . L.lexeme sp $ string "km/s for"
  duration <- natural
  void . L.lexeme sp $ string "seconds, but then must rest for"
  rest <- natural
  void . L.lexeme sp $ string "seconds."
  pure $ Reindeer{..}

data ReindeerState = Resting | Racing deriving (Show)

data RaceState = 
  RaceState 
    { timeLeft :: Natural
    , distanceCovered :: Natural
    , reindeerState :: ReindeerState
    } deriving (Show)

race :: Natural -> Reindeer -> (Reindeer, RaceState)
race time r = until stop run initialState
  where
    initialState :: (Reindeer, RaceState)
    initialState = (r, RaceState time 0 Racing)

    run :: (Reindeer, RaceState) -> (Reindeer, RaceState)
    run (rd@Reindeer{..}, RaceState{..}) = 
      case reindeerState of
        Racing -> if timeLeft >= duration
                    then (rd, RaceState (timeLeft - duration) ((speed * duration) + distanceCovered) Resting)
                    else (rd, RaceState 0 (speed * timeLeft + distanceCovered) Resting)
        Resting -> if timeLeft >= rest
                    then (rd, RaceState (timeLeft - rest) distanceCovered Racing)
                    else (rd, RaceState 0 distanceCovered Resting)

    stop :: (Reindeer, RaceState) -> Bool
    stop (_, RaceState{..}) = timeLeft == 0

raceReindeer :: Natural -> [Reindeer] -> [(Reindeer, Natural)]
raceReindeer n = sortBy cmpReindeer . fmap (second distanceCovered . (race n))
  where
    cmpReindeer :: (Reindeer, Natural) -> (Reindeer, Natural) -> Ordering
    cmpReindeer (_, n1) (_, n2) = n1 `compare` n2

newScoringRaceReindeer :: Natural -> [Reindeer] -> Map Text Natural
newScoringRaceReindeer time rs = fst $ until ((== time) . snd) run initialState
  where
    initialState :: (Map Text Natural, Natural)
    initialState = (M.fromList $ (\r -> (name r, 0)) <$> rs, 0)

    run :: (Map Text Natural, Natural) -> (Map Text Natural, Natural)
    run (m, t) = 
      let leaders = fmap (name . fst) . last . groupBy eqSnd . raceReindeer (t + 1) $ rs
      in (foldr (\k m' -> M.adjust (+1) k m') m leaders, t + 1)
  
eqSnd :: Eq b => (a, b) -> (a, b) -> Bool
eqSnd (_, x) (_, y) = x == y

findWinner :: Map Text Natural -> [(Text, Natural)]
findWinner = sortBy cmpReindeer . M.toList 
  where
    cmpReindeer :: (Text, Natural) -> (Text, Natural) -> Ordering
    cmpReindeer (_, n1) (_, n2) = n1 `compare` n2
