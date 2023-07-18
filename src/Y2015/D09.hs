module Y2015.D09 where

import Prelude hiding (or, and, not)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec (Parsec, some, empty)
import Text.Megaparsec.Char
import Control.Monad (void)
import Numeric.Natural (Natural)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid
import Data.List (permutations, sortBy)
import Data.Maybe

data Path = 
  Path 
    { from :: Text
    , to :: Text
    , distance :: Natural 
    } deriving (Eq, Show)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

loc :: Parsec Void Text Text
loc = fmap T.pack . L.lexeme sp $ some letterChar

toSym :: Parsec Void Text ()
toSym = void $ L.symbol sp "to"

eqSym :: Parsec Void Text ()
eqSym = void $ L.symbol sp "="

dist :: Parsec Void Text Natural
dist = fmap read . L.lexeme sp $ some digitChar

parsePath :: Parsec Void Text Path
parsePath = do
  from <- loc
  toSym
  to <- loc
  eqSym
  distance <- dist
  pure $ Path {..}

pathMap :: [Path] -> Map (Text, Text) Natural
pathMap = M.fromList . foldr addBothPaths []
  where
    addBothPaths :: Path -> [((Text, Text), Natural)] -> [((Text, Text), Natural)]
    addBothPaths Path{..} acc = 
        ((from, to), distance)
      : ((to, from), distance)
      : acc

allLocations :: [Path] -> Set Text
allLocations = foldr (\Path{..} acc -> S.insert to (S.insert from acc)) S.empty

routes :: [Path] -> [[Path]]
routes ps = catMaybes . fmap mkRoute . permutations . S.toList $ allLocations ps 
  where 
    m :: Map (Text, Text) Natural
    m = pathMap ps

    mkRoute :: [Text] -> Maybe [Path]
    mkRoute [] = Nothing
    mkRoute [_] = Nothing
    mkRoute (p1:p2:[]) = do  
      d <- M.lookup (p1,p2) m
      pure $ [Path p1 p2 d]
    mkRoute (p1:p2:xs) = do  
      d <- M.lookup (p1,p2) m
      rest <- mkRoute (p2:xs)
      pure $ Path p1 p2 d : rest

rankedRoutes :: [[Path]] -> [(Natural, [Path])]
rankedRoutes = sortBy cmpRouteDistance . fmap addDistance
  where 
    cmpRouteDistance :: (Natural, [Path]) -> (Natural, [Path]) -> Ordering
    cmpRouteDistance (distA, _) (distB, _) = distA `compare` distB 

    addDistance :: [Path] -> (Natural, [Path])
    addDistance p = (routeDistance p, p)

routeDistance :: [Path] -> Natural
routeDistance = getSum . foldMap (\Path{..} -> Sum distance)
