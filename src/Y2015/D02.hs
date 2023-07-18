module Y2015.D02 where

import Prelude hiding (length)
import Numeric.Natural (Natural)
import Data.Text (Text)
import Text.Megaparsec (Parsec)
import Data.Void
import Control.Applicative
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable1 (foldl1')
import Data.Foldable hiding (length)

data RectPrism =
  RectPrism 
    { width :: Natural
    , length :: Natural
    , height :: Natural
    } 

instance Show RectPrism where
  show (RectPrism{..}) = fold 
    [ show width
    , "x"
    , show length
    , "x"
    , show height
    ]

parseRectPrism :: Parsec Void Text RectPrism
parseRectPrism = do
  width :: Natural <- read <$> many digitChar
  _ <- char 'x'
  length :: Natural <- read <$> many digitChar
  _ <- char 'x'
  height :: Natural <- read <$> many digitChar
  pure RectPrism{..}

calcArea :: RectPrism -> (Natural, Natural, Natural)
calcArea RectPrism{..} = (width * length, length * height, height * width)

smallestSide :: (Natural, Natural, Natural) -> Natural
smallestSide (m, n, p) = m `min` n `min` p

paperRequired :: RectPrism -> Natural
paperRequired r = 
  let s@(a, b, c) = calcArea r
   in a * 2 
    + b * 2 
    + c * 2
    + smallestSide s

-- The ribbon required to wrap a present is the shortest distance around its sides, or the smallest perimeter of any one 
-- face. Each present also requires a bow made out of ribbon as well; the feet of ribbon required for the perfect bow is 
-- equal to the cubic feet of volume of the present. Don't ask how they tie the bow, though; they'll never tell.

calcPerims :: RectPrism -> NonEmpty Natural
calcPerims (RectPrism{..}) = (width * 2 + length * 2) 
  :| [ length * 2 + height * 2
     , width * 2 + height * 2
     ]

calcSmallestPerim :: RectPrism -> Natural
calcSmallestPerim = foldl1' min . calcPerims

calcBow :: RectPrism -> Natural
calcBow RectPrism{..} = width * height * length

ribbonRequired :: RectPrism -> Natural
ribbonRequired r = calcSmallestPerim r + calcBow r
