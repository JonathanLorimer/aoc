module Y2015.D18 where

import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Applicative (asum)
import Data.Bifunctor
import Numeric.Natural
import Data.Maybe
import Data.List

data Light = On | Off deriving (Show, Eq)

data Cell =
  Cell
    { columnNumber :: Natural
    , rowNumber :: Natural
    } deriving (Eq, Ord, Show)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

onSym :: Parsec Void Text ()
onSym = void $ L.symbol sp "#"

offSym :: Parsec Void Text ()
offSym = void $ L.symbol sp "."

lineP :: Parsec Void Text [Light]
lineP = many $ asum [ onSym $> On, offSym $> Off ]

mkLightMap :: [[Light]] -> Map Cell Light
mkLightMap ls = foldr aggCells M.empty withIdxs
  where
    withIdxs :: [(Natural, [(Natural, Light)])] 
    withIdxs = zip [0..] . fmap (zip [0..]) $ ls

    aggCells :: (Natural, [(Natural, Light)]) -> Map Cell Light -> Map Cell Light
    aggCells (colNum, rows) acc = 
      foldr (\(rowNum, light) -> M.insert (Cell colNum rowNum) light) acc rows

getBoundaryCell :: Map Cell Light -> Cell
getBoundaryCell = maximum . M.keys

(-.) :: Natural -> Natural -> Natural
(-.) n = fromMaybe 0 . minusNaturalMaybe n

adjacentCells 
  :: Cell -- ^ The maximum Cell for the matrix (should be the greatest row and column). Min bound is always 0
  -> Natural -- ^ Adjacency square size
  -> Cell -- ^ The Cell to get the adjacency for
  -> [Cell]
adjacentCells Cell { columnNumber = maxCol, rowNumber = maxRow } boxSize (Cell col row) =
  uncurry Cell <$> do
    col' <- withinBounds maxCol col
    row' <- withinBounds maxRow row
    pure (col', row')
  where
    withinBounds :: Natural -> Natural -> [Natural]
    withinBounds maxB subject = [subject -. boxSize .. min (subject + boxSize) maxB]

step 
  :: Cell -- ^ Boundary cell
  -> Map Cell Light 
  -> Map Cell Light  
step maxC m = M.mapWithKey calculateLight m
  where
    calculateLight :: Cell -> Light -> Light
    calculateLight c = 
      let ons = countOns $ mapMaybe (`M.lookup` m) (delete c $ adjacentCells maxC 1 c)
      in \case
          On -> if ons `elem` [2,3]
                  then On
                  else Off
          Off -> if ons == 3
                  then On
                  else Off

step' 
  :: Cell -- ^ Boundary cell
  -> Map Cell Light 
  -> Map Cell Light  
step' maxC m = M.mapWithKey calculateLight m
  where
    corners :: [Cell]
    corners = 
      [ Cell 0 0
      , Cell 0 (rowNumber maxC)
      , Cell (columnNumber maxC) 0
      , maxC
      ]

    calculateLight :: Cell -> Light -> Light
    calculateLight c = 
      let ons = countOns $ mapMaybe (`M.lookup` m) (delete c $ adjacentCells maxC 1 c)
      in if c `elem` corners
          then const On
          else \case
                  On -> if ons `elem` [2,3]
                          then On
                          else Off
                  Off -> if ons == 3
                          then On
                          else Off

countOns :: [Light] -> Natural
countOns = fromIntegral . length . fst . partition (== On)

setCellOn :: (Ord k) => k -> Map k Light -> Map k Light
setCellOn = M.adjust (const On)
