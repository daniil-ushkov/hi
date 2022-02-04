module HW3.T3 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, testSpec)

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (hasSameParseTreeWith, isPrettyPrintedAs)

taskTestSpec :: Spec
taskTestSpec = do
  "1 / 2"  `hasSameParseTreeWith` "div(1, 2)"
  "1 * 2"  `hasSameParseTreeWith` "mul(1, 2)"
  "1 + 2"  `hasSameParseTreeWith` "add(1, 2)"
  "1 - 2"  `hasSameParseTreeWith` "sub(1, 2)"
  "1 < 2"  `hasSameParseTreeWith` "less-than(1, 2)"
  "1 > 2"  `hasSameParseTreeWith` "greater-than(1, 2)"
  "1 >= 2" `hasSameParseTreeWith` "not-less-than(1, 2)"
  "1 <= 2" `hasSameParseTreeWith` "not-greater-than(1, 2)"
  "1 == 2" `hasSameParseTreeWith` "equals(1, 2)"
  "1 /= 2" `hasSameParseTreeWith` "not-equals(1, 2)"
  "1 && 2" `hasSameParseTreeWith` "and(1, 2)"
  "1 || 2" `hasSameParseTreeWith` "or(1, 2)"

  -- REPL session
  "2 + 2"                     `isPrettyPrintedAs` "4"
  "2 + 2 * 3"                 `isPrettyPrintedAs` "8"
  "(2 + 2) * 3"               `isPrettyPrintedAs` "12"
  "2 + 2 * 3 == (2 + 2) * 3"  `isPrettyPrintedAs` "false"
  "10 == 2*5 && 143 == 11*13" `isPrettyPrintedAs` "true"

taskTests :: IO TestTree
taskTests = testSpec "Task 3: Operators" taskTestSpec
