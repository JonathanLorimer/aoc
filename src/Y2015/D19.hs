module Y2015.D19 where

import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Data.Text as T
import Utils
import Data.Bifunctor
import Data.Either (partitionEithers)
import Data.Char (isUpper)

newtype Element = Element { getElement :: Text }
  deriving newtype (Eq, Ord, Show)

type Molecule = [Element]

newtype E = E { getE :: [Element] }
  deriving newtype (Eq, Ord, Show)

data MoleculeData =
  MoleculeData
    { electrons :: [E]
    , replacements :: Map Element [[Element]]
    , molecule :: [Element]
    } deriving (Eq, Ord, Show)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

assocSym :: Parsec Void Text ()
assocSym = void $ L.symbol sp "=>"

electronSym :: Parsec Void Text ()
electronSym = void $ L.symbol sp "e"

elementP :: Parsec Void Text Element
elementP = do
  x <- upperChar
  xs <- many lowerChar
  pure . Element . T.pack $ x:xs

{- First part -}

replacementP :: Parsec Void Text (Either E (Element, [Element]))
replacementP = do
  eElKey <- try (Right <$> elementP) <|> (Left <$> electronSym)
  sp
  assocSym
  sp
  molecules <- some elementP
  pure $ bimap (const $ E molecules) (,molecules) eElKey

moleculeDataP :: Parsec Void Text MoleculeData
moleculeDataP = do
  eElectronReplacements <- some $ replacementP <* newline
  let (electrons, replacements') = partitionEithers eElectronReplacements
      replacements = consolidate replacements'
  void newline
  molecule <- some elementP
  void eol
  pure $ MoleculeData {..}

consolidate :: [(Element, [Element])] -> Map Element [[Element]]
consolidate = foldr (uncurry $ upsert (:[]) (:)) M.empty

data Accum a b =
  Accum
    { orig :: a
    , result :: b
    }

singleSubstitution
  :: Map Element [[Element]]
  -> [Element]
  -> [[Element]]
singleSubstitution replacements molecules = result $
  foldr go (Accum [] []) molecules
  where
    go
      :: Element
      -> Accum [Element] [[Element]]
      -> Accum [Element] [[Element]]
    go m (Accum orig res) =
      case m `M.lookup` replacements of
        Nothing -> Accum (m:orig) $ (m :) <$> res
        Just subs ->
          let alternates = fmap (<> orig) subs
              partialReplacements = (m :) <$> res
          in Accum (m:orig) $ alternates <> partialReplacements

{- Second part -}

preProcess :: Text -> Text
preProcess =
    T.replace "Rn" "("
  . T.replace "Y" ","
  . T.replace "Ar" ")"

countSubstitutions :: Text -> Int
countSubstitutions t = T.foldl' go 0 t - 1
  where
    go :: Int -> Char -> Int
    go acc '(' = acc
    go acc ')' = acc
    go acc ',' = acc - 1
    go acc c = if isUpper c then acc + 1 else acc
