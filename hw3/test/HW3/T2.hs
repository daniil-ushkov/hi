module HW3.T2 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, testSpec)

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (isParsedTo, isPrettyPrintedAs)

taskTestSpec :: Spec
taskTestSpec = do
  "not"              `isParsedTo` HiExprValue (HiValueFunction HiFunNot)
  "and"              `isParsedTo` HiExprValue (HiValueFunction HiFunAnd)
  "or"               `isParsedTo` HiExprValue (HiValueFunction HiFunOr)
  "less-than"        `isParsedTo` HiExprValue (HiValueFunction HiFunLessThan)
  "greater-than"     `isParsedTo` HiExprValue (HiValueFunction HiFunGreaterThan)
  "equals"           `isParsedTo` HiExprValue (HiValueFunction HiFunEquals)
  "not-less-than"    `isParsedTo` HiExprValue (HiValueFunction HiFunNotLessThan)
  "not-greater-than" `isParsedTo` HiExprValue (HiValueFunction HiFunNotGreaterThan)
  "not-equals"       `isParsedTo` HiExprValue (HiValueFunction HiFunNotEquals)
  "if"               `isParsedTo` HiExprValue (HiValueFunction HiFunIf)

  "true"  `isParsedTo` HiExprValue (HiValueBool True)
  "false" `isParsedTo` HiExprValue (HiValueBool False)

  -- boolean algebra
  "not(true)"        `isPrettyPrintedAs` "false"
  "and(true, false)" `isPrettyPrintedAs` "false"
  "or(true, false)"  `isPrettyPrintedAs` "true"

  -- equality checking
  "equals(10, 10)"       `isPrettyPrintedAs` "true"
  "equals(false, false)" `isPrettyPrintedAs` "true"
  "equals(3, 10)"        `isPrettyPrintedAs` "false"
  "equals(1, true)"      `isPrettyPrintedAs` "false"

  -- comparison
  "less-than(3, 10)"       `isPrettyPrintedAs` "true"
  "less-than(false, true)" `isPrettyPrintedAs` "true"
  "less-than(false, 0)"    `isPrettyPrintedAs` "true"

  -- complements
  "equals(greater-than(1, 2), less-than(2, 1))"                     `isPrettyPrintedAs` "true"
  "equals(greater-than(add, mul), less-than(mul, add))"             `isPrettyPrintedAs` "true"

  "equals(not-equals(1, 2), not(equals(1, 2)))"                     `isPrettyPrintedAs` "true"
  "equals(not-equals(1, 1), not(equals(1, 1)))"                     `isPrettyPrintedAs` "true"

  "equals(not-less-than(1, 2), not(less-than(1, 2)))"               `isPrettyPrintedAs` "true"
  "equals(not-less-than(add, div), not(less-than(add, div)))"       `isPrettyPrintedAs` "true"

  "equals(not-greater-than(1, 2), not(greater-than(1, 2)))"         `isPrettyPrintedAs` "true"
  "equals(not-greater-than(add, mul), not(greater-than(add, mul)))" `isPrettyPrintedAs` "true"

  -- branching
  "if(true, 1, 2)"  `isPrettyPrintedAs` "1"
  "if(false, 1, 2)" `isPrettyPrintedAs` "2"

  -- REPL session
  "false"                                         `isPrettyPrintedAs` "false"
  "equals(add(2, 2), 4)"                          `isPrettyPrintedAs` "true"
  "less-than(mul(999, 99), 10000)"                `isPrettyPrintedAs` "false"
  "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" `isPrettyPrintedAs` "-1"
  "and(less-than(0, 1), less-than(1, 0))"         `isPrettyPrintedAs` "false"

  "if(true, add, mul)"          `isPrettyPrintedAs` "add"
  "if(true, add, mul)(10, 10)"  `isPrettyPrintedAs` "20"
  "if(false, add, mul)(10, 10)" `isPrettyPrintedAs` "100"

  "equals(add, add)" `isPrettyPrintedAs` "true"
  "equals(add, mul)" `isPrettyPrintedAs` "false"



taskTests :: IO TestTree
taskTests = testSpec "Task 2: Booleans and comparison" taskTestSpec
