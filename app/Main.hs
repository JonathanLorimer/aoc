module Main where

import Prelude hiding (lines, readFile, or, and, not)
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D07
import Text.Megaparsec
import Data.Map.Strict qualified as M
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  input <- lines <$> readFile "./input/Y2015/D07.txt"
  let mayOps = traverse (parseMaybe assignment) input
  case mayOps of
    Nothing -> putStrLn "Parse issue"
    Just ops -> do
      let env = M.fromList $ toTuple <$> ops
      let litA = Literal $ Pointer "a"
      eRes <- runExceptT $ evalStateT (eval litA) env 
      print $ "Result: " <> show eRes
