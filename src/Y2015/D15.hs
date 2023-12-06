module Y2015.D15 where

import Text.Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Monoid (Product (..))
import Data.Functor
import GHC.Natural (Natural)

data Ingredient =
  Ingredient 
    { name :: Text
    , properties :: Map Text Int
    } deriving (Show)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

colonSym :: Parsec Void Text ()
colonSym = void $ L.symbol sp ":"

commaSym :: Parsec Void Text ()
commaSym = void $ L.symbol sp ","

textP :: Parsec Void Text Text
textP = fmap T.pack . L.lexeme sp $ some letterChar

intP :: Parsec Void Text Int
intP = L.lexeme sp $ do
  isNegative <- isJust <$> optional (char '-')
  d :: Int <- read <$> some digitChar
  pure $ if isNegative then negate d else d

propertyP :: Parsec Void Text (Text, Int)
propertyP = liftA2 (,) textP intP

ingredientP :: Parsec Void Text Ingredient
ingredientP = do
  name <- textP
  colonSym
  p <- propertyP
  ps <- many $ commaSym >> propertyP
  let properties = M.fromList $ p:ps
  pure $ Ingredient{..}

removeCalories :: Map Text Int -> Map Text Int
removeCalories = M.delete "calories"

rmCalories :: Ingredient -> Ingredient
rmCalories Ingredient{..} = Ingredient name (removeCalories properties)

calculate :: [Int] -> Product Int
calculate = foldMap $ Product . min 0

distributions :: Natural -> Natural -> [[Natural]]
distributions _ 0 = [[]]
distributions lim' n' = go lim' (n' - 1) [[]]
  where
    go :: Natural -> Natural -> [[Natural]] -> [[Natural]]
    go lim 0 acc = fmap (lim:) acc
    go lim n acc = do
      curr <- [0..lim]
      nats <- acc
      go (lim - curr) (n - 1) [curr : nats]

countDistributions :: Natural -> Natural -> Natural
countDistributions 0 _ = 1
countDistributions _ 1 = 1
countDistributions n l = countDistributions (n - 1) l + countDistributions n (l - 1)

ingredientDistributions :: [Ingredient] -> [[(Ingredient, Natural)]]
ingredientDistributions is = zip is <$> distributions 100 (fromIntegral $ length is)

totalProperties :: (Ingredient, Natural) -> Ingredient
totalProperties (Ingredient { name, properties }, occurences) = 
  Ingredient
    { name = name
    , properties = (* (fromIntegral occurences)) <$> properties
    }

sumIngredients :: [Ingredient] -> Map Text Int
sumIngredients = foldr go M.empty
  where
    go :: Ingredient -> Map Text Int -> Map Text Int
    go i m = M.unionWith (+) m (properties i)

calculateCookieScore :: [Int] -> Int
calculateCookieScore = getProduct . foldMap (Product . max 0)

ingredientsToCookieScore :: [(Ingredient, Natural)] -> Int
ingredientsToCookieScore = calculateCookieScore . M.elems . M.delete "calories" . sumIngredients . fmap totalProperties

filterForCalories :: (Int -> Bool) -> [[(Ingredient, Natural)]] -> [[(Ingredient, Natural)]]
filterForCalories p = filter (maybe False p . M.lookup "calories" . sumIngredients . fmap totalProperties) 




