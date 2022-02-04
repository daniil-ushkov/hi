module HW3.T8 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
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
    "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000" `isPrettyPrintedAs` "parse-time(\"2021-12-15 00:16:40 UTC\")"
    "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")" `isPrettyPrintedAs` "3.351843755"
    "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60" `isPrettyPrintedAs` "parse-time(\"2022-01-01 00:00:00 UTC\")"


taskTests :: IO TestTree
taskTests = testSpec "Task 8: Date and time" taskTestSpec
