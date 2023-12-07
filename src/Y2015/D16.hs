module Y2015.D16 where

import Data.Functor
import Data.Map.Merge.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.List qualified as List
import Data.Bifunctor

data Sue a = Sue
  { number :: Natural
  , compounds :: a
  }
  deriving (Show, Functor)

giftCompounds :: Map Text Natural
giftCompounds =
  M.fromList
    [ ("children", 3)
    , ("cats", 7)
    , ("samoyeds", 2)
    , ("pomeranians", 3)
    , ("akitas", 0)
    , ("vizslas", 0)
    , ("goldfish", 5)
    , ("trees", 3)
    , ("cars", 2)
    , ("perfumes", 1)
    ]

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

colonSym :: Parsec Void Text ()
colonSym = void $ L.symbol sp ":"

commaSym :: Parsec Void Text ()
commaSym = void $ L.symbol sp ","

textP :: Parsec Void Text Text
textP = fmap T.pack . L.lexeme sp $ some letterChar

natP :: Parsec Void Text Natural
natP = fmap read . L.lexeme sp $ some digitChar

compoundP :: Parsec Void Text (Text, Natural)
compoundP = liftA2 (,) (textP <* colonSym) natP

sueP :: Parsec Void Text (Sue (Map Text Natural))
sueP = do
  void $ L.lexeme sp $ string "Sue"
  number <- natP
  colonSym
  p <- compoundP
  ps <- many $ commaSym >> compoundP
  let compounds = M.fromList $ p : ps
  pure $ Sue{..}

mergeSues :: (Text -> Natural -> Natural -> a) -> Map Text Natural -> [Sue (Map Text Natural)] -> [Sue (Map Text a)]
mergeSues f m ss = ss <&> fmap (merge dropMissing dropMissing (zipWithMatched f) m)

findMatches :: Map Text Natural -> [Sue (Map Text Natural)] -> [Sue (Map Text Bool)]
findMatches = mergeSues $ const (==)

findMatches' :: Map Text Natural -> [Sue (Map Text Natural)] -> [Sue (Map Text Bool)]
findMatches' = mergeSues \k giftAmount sueAmount ->
  case k of
    "cats" -> sueAmount > giftAmount
    "trees" -> sueAmount > giftAmount
    "pomeranians" -> sueAmount < giftAmount
    "goldfish" -> sueAmount < giftAmount
    _ -> sueAmount == giftAmount
  
filterMismatches :: [Sue (Map Text Bool)] -> [Sue [Text]]
filterMismatches =
  foldr
    ( \s acc ->
        if and . M.elems . compounds $ s
          then fmap M.keys s : acc
          else acc
    )
    []

findMax :: [Sue [Text]] -> Sue [Text]
findMax = List.maximumBy (\s1 s2 -> compare (length . compounds $ s1) (length . compounds $ s2))
