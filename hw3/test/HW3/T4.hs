{-# LANGUAGE OverloadedStrings #-}

module HW3.T4 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (Success), isEvaluatedTo, isParsedTo, isPrettyPrintedAs)

taskTestSpec :: Spec
taskTestSpec = do
  describe "identifiers parsing" $ do
    "length"   `isParsedTo` HiExprValue (HiValueFunction HiFunLength)
    "to-upper" `isParsedTo` HiExprValue (HiValueFunction HiFunToUpper)
    "to-lower" `isParsedTo` HiExprValue (HiValueFunction HiFunToLower)
    "trim"     `isParsedTo` HiExprValue (HiValueFunction HiFunTrim)

  describe "null literal" $ do
    "null" `isParsedTo` HiExprValue HiValueNull

  describe "string literals" $ do
    "\"hello\""          `isParsedTo` HiExprValue (HiValueString "hello")
    "\"42\""             `isParsedTo` HiExprValue (HiValueString "42")
    "\"header\nfooter\"" `isParsedTo` HiExprValue (HiValueString "header\nfooter")

  describe "evaluation" $ do
    "length(\"Hello World\")"   `isEvaluatedTo` Success (HiValueNumber 11)
    "to-upper(\"Hello World\")" `isEvaluatedTo` Success (HiValueString "HELLO WORLD")
    "to-lower(\"Hello World\")" `isEvaluatedTo` Success (HiValueString "hello world")
    "reverse(\"stressed\")"     `isEvaluatedTo` Success (HiValueString "desserts")
    "trim(\" Hello World \")"   `isEvaluatedTo` Success (HiValueString "Hello World")

  describe "operators for strings" $ do
    "\"Hello\" + \"World\""   `isEvaluatedTo` Success (HiValueString "HelloWorld")
    "\"Cat\" * 5"             `isEvaluatedTo` Success (HiValueString "CatCatCatCatCat")
    "\"/home/user\" / \"hi\"" `isEvaluatedTo` Success (HiValueString "/home/user/hi")

  describe "index in bounds" $ do
    "\"Hello World\"(0)" `isEvaluatedTo` Success (HiValueString "H")
    "\"Hello World\"(7)" `isEvaluatedTo` Success (HiValueString "o")

  describe "index out of bounds" $ do
    "\"Hello World\"(-1)" `isEvaluatedTo` Success HiValueNull
    "\"Hello World\"(99)" `isEvaluatedTo` Success HiValueNull

  describe "slices" $ do
    "\"Hello World\"(0, 5)" `isEvaluatedTo` Success (HiValueString "Hello")
    "\"Hello World\"(2, 4)" `isEvaluatedTo` Success (HiValueString "ll")

  describe "advanced" $ do
    "\"Hello World\"(0, -4)"  `isEvaluatedTo` Success (HiValueString "Hello W")
    "\"Hello World\"(-4, -1)" `isEvaluatedTo` Success (HiValueString "orl")

    "\"Hello, World\"(2, null)"  `isEvaluatedTo` Success (HiValueString "llo, World")
    "\"Hello, World\"(null, 5)"  `isEvaluatedTo` Success (HiValueString "Hello")

  describe "from REPL session" $ do
    "to-upper(\"what a nice language\")(7, 11)" `isPrettyPrintedAs` "\"NICE\""
    "\"Hello\" == \"World\""                    `isPrettyPrintedAs` "false"
    "length(\"Hello\" + \"World\")"             `isPrettyPrintedAs` "10"
    "length(\"hehe\" * 5) / 3"                  `isPrettyPrintedAs` "6 + 2/3"

taskTests :: IO TestTree
taskTests = testSpec "Task 4: Strings and slices" taskTestSpec
