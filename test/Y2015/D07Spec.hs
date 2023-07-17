module Y2015.D07Spec where
  
import Prelude hiding (lines, readFile, or, and, not)
import Test.Hspec
import Data.Text.IO (readFile)
import Data.Text (lines)
import Y2015.D07
import Text.Megaparsec
import Data.Map.Strict qualified as M
import Utils (assertRight, assertJustMsg)
import Data.Maybe
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Except (runExceptT)

spec :: Spec
spec = do
  describe "parser" $ do
    it "signal" $ do
      runParser signal "inline" "24 -> b" `shouldBe` (Right 24)
    it "pointer" $ do
      runParser pointer "inline" "xy -> b" `shouldBe` (Right "xy")
    it "value" $ do
      runParser value "inline" "24 -> b" `shouldBe` (Right $ Signal 24)
      runParser value "inline" "xy -> b" `shouldBe` (Right $ Pointer "xy")
    it "literal" $ do
      runParser literal "inline" "24 -> b" `shouldBe` (Right . Literal $ Signal 24)
      runParser literal "inline" "xy -> b" `shouldBe` (Right . Literal $ Pointer "xy")
    it "not" $ do
      runParser operation "inline" "NOT dq -> dr" 
        `shouldBe` (Right . Not . Literal . Pointer $ "dq")
    it "and" $ do
      runParser operation "inline" "il AND in -> io" 
        `shouldBe` 
          (Right $ And 
            (Literal . Pointer $ "il") 
            (Literal . Pointer $ "in")
          )
      runParser operation "inline" "1 AND 2 -> io" 
        `shouldBe` 
          (Right $ And 
            (Literal . Signal $ 1) 
            (Literal . Signal $ 2)
          )
      runParser operation "inline" "1 AND in -> io" 
        `shouldBe` 
          (Right $ And 
            (Literal . Signal $ 1) 
            (Literal . Pointer $ "in")
          )
    it "or" $ do
      runParser operation "inline" "du OR dt -> dv" 
        `shouldBe` 
          (Right $ Or 
            (Literal . Pointer $ "du") 
            (Literal . Pointer $ "dt")
          )
    it "rshift" $ do
      runParser operation "inline" "b RSHIFT 2 -> d" 
        `shouldBe` 
          (Right $ RShift 
            (Literal . Pointer $ "b") 
            (Literal . Signal $ 2)
          )
    it "lshift" $ do
      runParser operation "inline" "jm LSHIFT 1 -> kg" 
        `shouldBe` 
          (Right $ LShift 
            (Literal . Pointer $ "jm") 
            (Literal . Signal $ 1)
          )
    it "operation" $ do
      runParser assignment "inline" "24 -> b" 
        `shouldBe` (Right $ Assignment "b" (Literal $ Signal 24))
      runParser assignment "inline" "xy -> b" 
        `shouldBe` (Right $ Assignment "b" (Literal $ Pointer "xy"))
      runParser assignment "inline" "NOT dq -> dr" 
        `shouldBe` (Right $ Assignment "dr" (Not . Literal $ Pointer "dq"))
      runParser assignment "inline" "il AND in -> io" 
        `shouldBe` 
          (Right $ 
            Assignment "io"
              (And 
                (Literal $ Pointer "il") 
                (Literal $ Pointer "in")
              )
          )
      runParser assignment "inline" "du OR dt -> dv" 
        `shouldBe`
          (Right $ 
            Assignment "dv"
              (Or 
                (Literal $ Pointer "du") 
                (Literal $ Pointer "dt")
              )
          )
      runParser assignment "inline" "b RSHIFT 2 -> d" 
        `shouldBe`
          (Right $ 
            Assignment "d"
              (RShift 
                (Literal $ Pointer "b") 
                (Literal $ Signal 2)
              )
          )
      runParser assignment "inline" "jm LSHIFT 1 -> kg" 
        `shouldBe`
          (Right $ 
            Assignment "kg"
              (LShift 
                (Literal $ Pointer "jm") 
                (Literal $ Signal 1)
              )
          )

  describe "Some Assembly Required pt.1" $ do
    it "result" $ do
      input <- lines <$> readFile "./input/Y2015/D07.txt"
      let parsedInput = parseMaybe assignment <$> input
      print $ filter (isNothing . fst) (zip parsedInput input)
      ops <- assertJustMsg "trouble parsing input" $ sequence parsedInput 
      let env = M.fromList $ toTuple <$> ops
      let litA = Literal $ Pointer "a"
      eRes <- runExceptT $ evalStateT (eval litA) env 
      res <- assertRight eRes
      res `shouldBe` 0
  --     
  -- describe "Some Assembly Required pt.2" $ do
  --   xit "result" $ do
  --     _input <- lines <$> readFile "./input/Y2015/D07.txt"
  --     pending
