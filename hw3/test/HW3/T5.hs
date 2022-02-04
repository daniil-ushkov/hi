{-# LANGUAGE OverloadedStrings #-}

module HW3.T5 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import qualified Data.Sequence as S

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (Success), hasSameParseTreeWith, isEvaluatedTo, isParsedTo,
                  isPrettyPrintedAs)

taskTestSpec :: Spec
taskTestSpec = do
    describe "identifiers parsing" $ do
      "list"   `isParsedTo` HiExprValue (HiValueFunction HiFunList)
      "range"  `isParsedTo` HiExprValue (HiValueFunction HiFunRange)
      "fold"   `isParsedTo` HiExprValue (HiValueFunction HiFunFold)

    describe "list literal" $ do
      "[1, 2, 3]" `isParsedTo` HiExprApply (HiExprValue (HiValueFunction HiFunList))
        [HiExprValue (HiValueNumber 1), HiExprValue (HiValueNumber 2), HiExprValue (HiValueNumber 3)]
      "[]"        `isParsedTo` HiExprApply (HiExprValue (HiValueFunction HiFunList)) []

      "[add, \"aboba\", 42]" `hasSameParseTreeWith` "list(add, \"aboba\", 42)"

    describe "evaluation" $ do
      "list(1, 2, 3)"  `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueNumber 1, HiValueNumber 2, HiValueNumber 3]))
      "range(5, 10.3)" `isEvaluatedTo` Success (HiValueList (S.fromList $ HiValueNumber <$> [5, 6, 7, 8, 9, 10]))
      "fold(add, [11, 22, 33])"  `isEvaluatedTo` Success (HiValueNumber 66)
      "fold(mul, [11, 22, 33])"  `isEvaluatedTo` Success (HiValueNumber 7986)
      "fold(div, [11, 22, 33])" `isEvaluatedTo` Success (HiValueNumber (1 / 66))

    describe "overload existing operations to work on lists" $ do
      "length([1, true, \"Hello\"])"  `isEvaluatedTo` Success (HiValueNumber 3)
      "reverse([1, true, \"Hello\"])" `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueString "Hello", HiValueBool True, HiValueNumber 1]))
      "[1, 2] + [3, 4, 5]"            `isEvaluatedTo` Success (HiValueList (S.fromList $ HiValueNumber <$> [1, 2, 3, 4, 5]))
      "[0, \"x\"] * 3"                `isEvaluatedTo` Success (HiValueList (S.fromList
        [HiValueNumber 0, HiValueString "x", HiValueNumber 0, HiValueString "x", HiValueNumber 0, HiValueString "x"]))

    describe "from REPL session" $ do
      "list(1, 2, 3, 4, 5)"                          `isPrettyPrintedAs` "[ 1, 2, 3, 4, 5 ]"
      "fold(add, [2, 5] * 3)"                        `isPrettyPrintedAs` "21"
      "fold(mul, range(1, 10))"                      `isPrettyPrintedAs` "3628800"
      "[0, true, false, \"hello\", \"world\"](2, 4)" `isPrettyPrintedAs` "[ false, \"hello\" ]"
      "reverse(range(0.5, 70/8))"                    `isPrettyPrintedAs` "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"

    describe "from Slack" $ do
      "fold(\"aboba\", [0, 5])" `isEvaluatedTo` Success (HiValueString "aboba")
      "\"suicide\"(4,100)" `isEvaluatedTo` Success (HiValueString "ide")
taskTests :: IO TestTree
taskTests = testSpec "Task 5: Lists and folds" taskTestSpec
