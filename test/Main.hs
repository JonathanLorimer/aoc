module Main where

import Data.Maybe (fromMaybe)
import Spec qualified
import System.Environment (lookupEnv)
import Test.Hspec (parallel)
import Test.Hspec.Core.Runner (Config (..))
import Test.Hspec.Runner as TR (defaultConfig, hspecWith)
import Text.Read (readMaybe)
import Test.Hspec.Api.Formatters.V2 (useFormatter)
import Test.Hspec.Api.Formatters.V2 (specdoc)

main :: IO ()
main = do
  mText <- lookupEnv "TEST_CONCURRENCY"
  let maxResources :: Int
      maxResources = fromMaybe 8 (mText >>= readMaybe)
  let cfg = useFormatter ("specdoc", specdoc)
        TR.defaultConfig
          { configConcurrentJobs = Just maxResources
          }
  hspecWith cfg (parallel Spec.spec)
