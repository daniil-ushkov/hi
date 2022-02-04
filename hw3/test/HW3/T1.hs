module HW3.T1 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import Data.Ratio ((%))

import HW3.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (EvalError, Success), isEvaluatedTo, isParsedTo,
                  isPrettyPrintedAs)

taskTestSpec :: Spec
taskTestSpec = do
    describe "Subtask 2" $ do
      -- Arithmetic operations identifiers parsing
      "div" `isParsedTo` HiExprValue (HiValueFunction HiFunDiv)
      "mul" `isParsedTo` HiExprValue (HiValueFunction HiFunMul)
      "add" `isParsedTo` HiExprValue (HiValueFunction HiFunAdd)
      "sub" `isParsedTo` HiExprValue (HiValueFunction HiFunSub)

      -- Numeric literals
      "2"      `isParsedTo` HiExprValue (HiValueNumber 2)
      "3.14"   `isParsedTo` HiExprValue (HiValueNumber 3.14)
      "-1.618" `isParsedTo` HiExprValue (HiValueNumber (-1.618))
      "1.2e5"  `isParsedTo` HiExprValue (HiValueNumber 1.2e5)

      -- Function Application
      "div(add(10, 15.1), 3)" `isParsedTo`
        HiExprApply (HiExprValue (HiValueFunction HiFunDiv))
          [
            HiExprApply (HiExprValue (HiValueFunction HiFunAdd))
            [
              HiExprValue (HiValueNumber (10 % 1)),
              HiExprValue (HiValueNumber (151 % 10))
            ],
          HiExprValue (HiValueNumber (3 % 1))
          ]

    describe "Subtask 3" $ do
      "add(500, 12)" `isEvaluatedTo` Success (HiValueNumber 512)
      "sub(10, 100)" `isEvaluatedTo` Success (HiValueNumber (-90))
      "mul(23, 768)" `isEvaluatedTo` Success (HiValueNumber 17664)
      "div(57, 190)" `isEvaluatedTo` Success (HiValueNumber 0.3)

      "div(add(mul(2, 5), 1), sub(11,6))" `isEvaluatedTo` Success (HiValueNumber 2.2)

      "sub(1)"            `isEvaluatedTo` EvalError HiErrorArityMismatch
      "sub(1, 2, 3)"      `isEvaluatedTo` EvalError HiErrorArityMismatch
      "div(1, 0)"         `isEvaluatedTo` EvalError HiErrorDivideByZero
      "div(1, sub(5, 5))" `isEvaluatedTo` EvalError HiErrorDivideByZero
      "15(2)"             `isEvaluatedTo` EvalError HiErrorInvalidFunction
      "sub(10, add)"      `isEvaluatedTo` EvalError HiErrorInvalidArgument

      "100"                     `isPrettyPrintedAs` "100"
      "-15"                     `isPrettyPrintedAs` "-15"
      "add(100, -15)"           `isPrettyPrintedAs` "85"
      "add(3, div(14, 100))"    `isPrettyPrintedAs` "3.14"
      "div(10, 3)"              `isPrettyPrintedAs` "3 + 1/3"
      "sub(mul(201, 11), 0.33)" `isPrettyPrintedAs` "2210.67"

    describe "from Slack" $ do
      "div ( add ( 10 , 15.1 ) , 3 )" `isPrettyPrintedAs` "8 + 11/30"

taskTests :: IO TestTree
taskTests = testSpec "Task 1: Numbers and arithmetic" taskTestSpec
