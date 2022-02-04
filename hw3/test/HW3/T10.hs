module HW3.T10 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (Success), hasSameParseTreeWith, isEvaluatedTo, isParsedTo,
                  isPrettyPrintedAs)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Sequence as S

taskTestSpec :: Spec
taskTestSpec = do
  describe "from REPL" $ do
    "echo"                   `isPrettyPrintedAs` "echo"
    "1 / 0"                  `isPrettyPrintedAs` "HiErrorDivideByZero"
    "\"Hello\"(0) || \"Z\""  `isPrettyPrintedAs` "\"H\""
    "\"Hello\"(99) || \"Z\"" `isPrettyPrintedAs` "\"Z\""
    "if(2 == 2, 42, 1 / 0)"  `isPrettyPrintedAs` "42"
    "true || (1 / 0)"        `isPrettyPrintedAs` "true"
    "false && (1 / 0)"       `isPrettyPrintedAs` "false"
    "[# 00 ff #] && (1 / 0)" `isPrettyPrintedAs` "HiErrorDivideByZero"

taskTests :: IO TestTree
taskTests = testSpec "Task 10: Short-circuit evaluation" taskTestSpec
