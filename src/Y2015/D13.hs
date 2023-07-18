module Y2015.D13 where
import Data.Text (Text)
import Text.Megaparsec (Parsec, (<|>), some, empty, try)
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (letterChar, digitChar, space1, string, char)
import qualified Data.Text as T
import Control.Monad (void)
import qualified Data.Set as S
import Data.List (permutations, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid

data Arrangement = 
  Arrangement 
    { sitter :: Text
    , seatMate :: Text
    , happiness :: Int
    } deriving (Eq, Ord, Show)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

entity :: Parsec Void Text Text
entity = fmap T.pack . L.lexeme sp $ some letterChar

hap :: Parsec Void Text Int
hap = fmap read . L.lexeme sp $ some digitChar

data Sign = Positive | Negative 

gain :: Parsec Void Text Sign
gain = L.symbol sp "gain" >> pure Positive

lose :: Parsec Void Text Sign
lose = L.symbol sp "lose" >> pure Negative

arrangement :: Parsec Void Text Arrangement
arrangement = do
  sitter <- entity
  void . L.lexeme sp $ string "would"
  sign <- try gain <|> lose
  happiness <- hap
  void . L.lexeme sp $ string "happiness units by sitting next to"
  seatMate <- entity
  void $ char '.'
  pure . Arrangement sitter seatMate $
    case sign of 
      Positive -> happiness
      Negative -> negate happiness

rankedArrangements :: [Arrangement] -> [(Int, [Arrangement])]
rankedArrangements a = rankArrangements $ arrangements (arrangementMap a) (possibleSeatingArrangements a)

arrangementMap :: [Arrangement] -> Map (Text, Text) Int
arrangementMap = M.fromList . foldr (\Arrangement{..} acc -> ((sitter, seatMate), happiness):acc) []

arrangements :: Map (Text,Text) Int -> [[Text]] -> [[Arrangement]]
arrangements m = catMaybes . fmap (\xs -> mkArrangement (head xs) (last xs : xs))
  where 
    mkArrangement 
      :: Text -- ^ First element of the list 
      -> [Text] -- ^ List
      -> Maybe [Arrangement]
    mkArrangement _ [] = Nothing
    mkArrangement _ (_:[]) = Nothing
    mkArrangement mate2 (mate1:sitter:[]) = do
      h1 <- M.lookup (sitter,mate1) m
      h2 <- M.lookup (sitter,mate2) m
      pure $ [Arrangement sitter mate1 h1, Arrangement sitter mate2 h2 ]
    mkArrangement first (mate1:sitter:mate2:xs) = do  
      h1 <- M.lookup (sitter,mate1) m
      h2 <- M.lookup (sitter,mate2) m
      rest <- mkArrangement first (sitter:mate2:xs)
      pure $ Arrangement sitter mate1 h1 : Arrangement sitter mate2 h2 : rest

allSitters :: [Arrangement] -> S.Set Text
allSitters = foldr (S.insert . sitter) S.empty 

possibleSeatingArrangements :: [Arrangement] -> [[Text]]
possibleSeatingArrangements = permutations . S.toList . allSitters

rankArrangements :: [[Arrangement]] -> [(Int, [Arrangement])]
rankArrangements = sortBy cmpTotalHappiness . fmap addHappiness
  where 
    cmpTotalHappiness :: (Int, [Arrangement]) -> (Int, [Arrangement]) -> Ordering
    cmpTotalHappiness (hA, _) (hB, _) = hA `compare` hB 

    addHappiness :: [Arrangement] -> (Int, [Arrangement])
    addHappiness a = (totalHappiness a, a)

totalHappiness :: [Arrangement] -> Int
totalHappiness = getSum . foldMap (\Arrangement{..} -> Sum happiness)
