module Y2015.D07 where

import Prelude hiding (or, and, not)
import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative
import Data.Map.Strict (Map)
import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Map as M
import Utils (note)
import Data.Bits hiding (And)
import Text.Megaparsec.Char.Lexer (lexeme)
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
import Text.Megaparsec.Byte.Lexer (symbol)
import Control.Monad.State.Strict (StateT, MonadState (..), gets, modify)
import Data.Map.Strict (insert)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.IO.Class
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char
import Control.Monad.Combinators (choice)
import Control.Monad (void, join, when)

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
  [ Signal <$> signal 
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
  op <- asum [ operation , literal ]
  void $ symbol sp "->"
  var <- pointer
  pure $ Assignment var op

isLiteralSignal :: Operation Value -> Bool
isLiteralSignal (Literal (Signal _)) = True
isLiteralSignal _ = False

countLiterals :: Map Text (Operation Value) -> Int
countLiterals = length . filter isLiteralSignal . fmap snd . M.toList

eval' :: Bool -> (Operation Value) -> StateT (Map Text (Operation Value)) (ExceptT Text IO) Word16
eval' withLogging op = 
  case op of
    Literal (Signal u16) -> do
      when withLogging $ liftIO $ putStrLn $ "literal: " <> show u16
      pure u16
    Literal (Pointer var) -> do 
      when withLogging $ liftIO $ putStrLn $ "evaluating pointer: " <> T.unpack var
      val <- join . gets $ liftEither . note ("Could not find " <> var <> " in env") . M.lookup var 
      result <- eval' withLogging val
      when withLogging $ liftIO $ putStrLn $ T.unpack var <> ": " <> show result
      modify $ insert var (Literal $ Signal result)
      when withLogging $ liftIO . putStrLn . mappend "Literal Signal Count: " . show . countLiterals =<< get
      pure result
    Not oper -> do
      when withLogging $ liftIO $ putStrLn $ "Not: " <> show op
      val <- eval' withLogging oper
      pure $ complement val
    And a b -> do
      when withLogging $ liftIO $ putStrLn $ "And: " <> show op
      valA <- eval' withLogging a
      valB <- eval' withLogging b
      pure $ valA .&. valB
    Or a b -> do
      when withLogging $ liftIO $ putStrLn $ "Or: " <> show op
      valA <- eval' withLogging a
      valB <- eval' withLogging b
      pure $ valA .|. valB
    LShift var bits -> do
      when withLogging $ liftIO $ putStrLn $ "LShift: " <> show op
      varValue <- eval' withLogging var
      bitsValue <- fromIntegral <$> eval' withLogging bits
      pure $ shiftL varValue bitsValue
    RShift var bits -> do
      when withLogging $ liftIO $ putStrLn $ "RShift: " <> show op
      varValue <- eval' withLogging  var
      bitsValue <- fromIntegral <$> eval' withLogging bits
      pure $ shiftR varValue bitsValue

eval :: (Operation Value) -> StateT (Map Text (Operation Value)) (ExceptT Text IO) Word16
eval = eval' False
