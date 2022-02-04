{-# LANGUAGE OverloadedStrings #-}

module HW3.T11 where

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
  describe "identifiers parsing" $ do
    "count"   `isParsedTo` HiExprValue (HiValueFunction HiFunCount)
    "keys"    `isParsedTo` HiExprValue (HiValueFunction HiFunKeys)
    "values"  `isParsedTo` HiExprValue (HiValueFunction HiFunValues)
    "invert"  `isParsedTo` HiExprValue (HiValueFunction HiFunInvert)

  describe "dict literals" $ do
    "{ \"width\": 120, \"height\": 80 }" `isEvaluatedTo` Success (HiValueDict (M.fromList
      [(HiValueString "width", HiValueNumber 120), (HiValueString "height", HiValueNumber 80)]))
    "{ 1: true, 3: true, 4: false }"     `isEvaluatedTo` Success (HiValueDict (M.fromList
      [(HiValueNumber 1, HiValueBool True), (HiValueNumber 3, HiValueBool True), (HiValueNumber 4, HiValueBool False)]))

  describe "dict dot access" $ do
    "{ \"width\": 120, \"height\": 80 }.width" `hasSameParseTreeWith` "{ \"width\": 120, \"height\": 80 }(\"width\")"

  describe "evaluation" $ do
    "{ \"width\": 120, \"height\": 80 }(\"width\")" `isEvaluatedTo` Success (HiValueNumber 120)
    "keys({ \"width\": 120, \"height\": 80 })"      `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueString "height", HiValueString "width"]))
    "values({ \"width\": 120, \"height\": 80 })"    `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueNumber 80, HiValueNumber 120]))
    "count(\"XXXOX\")"                              `isEvaluatedTo` Success (HiValueDict (M.fromList [(HiValueString "O", HiValueNumber 1), (HiValueString "X", HiValueNumber 4)]))
    "count([# 58 58 58 4f 58 #])"                   `isEvaluatedTo` Success (HiValueDict (M.fromList [(HiValueNumber 79, HiValueNumber 1), (HiValueNumber 88, HiValueNumber 4)]))
    "count([true, true, false, true])"              `isEvaluatedTo` Success (HiValueDict (M.fromList [(HiValueBool False, HiValueNumber 1), (HiValueBool True, HiValueNumber 3)]))
    "invert({ \"x\": 1, \"y\": 2, \"z\": 1 })"      `isPrettyPrintedAs` "{ 1: [ \"z\", \"x\" ], 2: [ \"y\" ] }"

  describe "from REPL session" $ do
    "count(\"Hello World\").o"                    `isPrettyPrintedAs` "2"
    "invert(count(\"big blue bag\"))"             `isPrettyPrintedAs` "{ 1: [ \"u\", \"l\", \"i\", \"e\", \"a\" ], 2: [ \"g\", \" \" ], 3: [ \"b\" ] }"
    "fold(add, values(count(\"Hello, World!\")))" `isPrettyPrintedAs` "13"

  describe "from Slack" $ do
    "{ \"my-awesome-key\": 42 }.my-awesome-key" `isEvaluatedTo` Success (HiValueNumber 42)

taskTests :: IO TestTree
taskTests = testSpec "Task 11: Dictionaries" taskTestSpec
