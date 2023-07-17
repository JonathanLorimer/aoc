module Y2015.D07 where

import Prelude hiding (or, and, not)
import Numeric.Natural
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Text.Megaparsec (Parsec, MonadParsec (try), choice)
import Data.Void (Void)
import Text.Megaparsec.Char (string, digitChar, char, space, lowerChar, spaceChar, space1)
import Control.Applicative
import Control.Monad (void)
import Data.Map.Strict (Map, adjust)
import Data.Maybe
import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Map as M
import Utils (note)
import Data.Bits hiding (And)
import qualified Debug.Trace as Debug
import Text.Megaparsec.Char.Lexer (lexeme)
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
import Text.Megaparsec.Byte.Lexer (symbol)
import Control.Monad.State.Strict (StateT, MonadState (..), MonadTrans (..))
import Data.Map.Strict (insert)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.IO.Class

data Value = Signal Word16 | Pointer Text
  deriving (Show, Eq)

data Operation a = Literal a
                 | Not (Operation a)
                 | And (Operation a) (Operation a)
                 | Or (Operation a) (Operation a)
                 | LShift (Operation a) (Operation a)
                 | RShift (Operation a) (Operation a)
  deriving (Show, Eq, Functor)

sp :: Parsec Void Text ()
sp = L.space space1 empty empty

signal :: Parsec Void Text Word16
signal = fmap read . lexeme sp $ some digitChar

pointer :: Parsec Void Text Text
pointer = fmap T.pack . lexeme sp $ some lowerChar

value :: Parsec Void Text Value
value = choice 
  [ Signal <$> try signal 
  , Pointer <$> pointer
  ]

literal :: Parsec Void Text (Operation Value)
literal = Literal <$> value

operation :: Parsec Void Text (Operation Value)
operation = makeExprParser literal
  [ [ Prefix $ Not        <$ symbol sp "NOT"   ]
  , [ InfixL $ And        <$ symbol sp "AND"    ]
  , [ InfixL $ Or         <$ symbol sp "OR"    ]
  , [ InfixL $ LShift     <$ symbol sp "LSHIFT"  ]
  , [ InfixL $ RShift     <$ symbol sp "RSHIFT"  ]
  ]

data Assignment = Assignment Text (Operation Value)
  deriving (Eq, Show)

toTuple :: Assignment -> (Text, Operation Value)
toTuple (Assignment t op) = (t, op)

assignment :: Parsec Void Text Assignment
assignment = do
  op <- asum [ try operation , literal ]
  symbol sp "->"
  var <- pointer
  pure $ Assignment var op

-- type MonadBitLang = StateT (Map Text (Operation Value)) (Either Text) Word16

isLiteralSignal :: Operation Value -> Bool
isLiteralSignal (Literal (Signal _)) = True
isLiteralSignal _ = False

countLiterals :: Map Text (Operation Value) -> Int
countLiterals = length . filter isLiteralSignal . fmap snd . M.toList

eval :: (Operation Value) -> StateT (Map Text (Operation Value)) (ExceptT Text IO) Word16
eval op = 
  case op of
    Literal (Signal u16) -> do
      liftIO $ print $ "literal: " <> show u16
      pure u16
    Literal (Pointer var) -> do 
      env <- get
      liftIO $ putStrLn $ "evaluating pointer: " <> T.unpack var
      val <- liftEither $ note ("Could not find " <> var <> " in env") (M.lookup var env)
      result <- eval val
      liftIO $ putStrLn $ T.unpack var <> ": " <> show result
      let newState = insert var (Literal $ Signal result) env
      put newState
      liftIO $ putStrLn $ "Literal Signal Count: " <> show (countLiterals newState)
      pure result
    Not op -> do
      liftIO $ putStrLn $ "Not: " <> show op
      val <- eval op
      pure $ complement val
    And a b -> do
      liftIO $ putStrLn $ "And: " <> show op
      valA <- eval a
      valB <- eval b
      pure $ valA .&. valB
    Or a b -> do
      liftIO $ putStrLn $ "Or: " <> show op
      valA <- eval a
      valB <- eval b
      pure $ valA .|. valB
    LShift var bits -> do
      liftIO $ putStrLn $ "LShift: " <> show op
      varValue <- eval var
      bitsValue <- fromIntegral <$> eval bits
      pure $ shiftL varValue bitsValue
    RShift var bits -> do
      liftIO $ putStrLn $ "RShift: " <> show op
      varValue <- eval var
      bitsValue <- fromIntegral <$> eval bits
      pure $ shiftR varValue bitsValue
