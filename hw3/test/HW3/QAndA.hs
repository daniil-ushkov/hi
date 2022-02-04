{-# LANGUAGE OverloadedStrings #-}

module HW3.QAndA where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy, testSpec)

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (EvalError, ParseError, Success), allPerms, assertExecution',
                  hasSameParseTreeWith, isEvaluatedTo, isPrettyPrintedAs, unsafeRunHi)

import Control.Monad (forM_)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Sequence as S

import Prettyprinter (pretty)

taskTestSpec :: Spec
taskTestSpec = do
  "(div(1))(10)" `isEvaluatedTo` EvalError HiErrorArityMismatch

  describe "errors priority in single function call" $ do
    "15(1, mul, 1 / 0)" `isEvaluatedTo` EvalError HiErrorInvalidFunction        -- 1, 2?, 3?, 4
    "div(sub, 2, 0)"    `isEvaluatedTo` EvalError HiErrorArityMismatch          -- 2, 3, 4?
    "div(sub, 0)"       `isEvaluatedTo` EvalError HiErrorInvalidArgument        -- 3, 4
    "3 / 0"             `isEvaluatedTo` EvalError HiErrorDivideByZero           -- 4

  "[# 00 ff #](1)" `isEvaluatedTo` Success (HiValueNumber 255)

  "if(true, 1, 1/0)" `isEvaluatedTo` Success (HiValueNumber 1)

  "add.hello-world" `hasSameParseTreeWith` "add(\"hello-world\")"

  "[# add(1,15) #]" `isEvaluatedTo` ParseError

  "\"abc\"(1, 2.2)" `isEvaluatedTo` EvalError HiErrorInvalidArgument

  describe "Valid and invalid bytes literals" $ do
    "[# 01 23 #]" `isEvaluatedTo` Success (HiValueBytes $ BS.pack [1, 35])
    "[# 1 23 #]"  `isEvaluatedTo` ParseError
    "[# 1 2 3 #]" `isEvaluatedTo` ParseError

  "[1 + 2, 3 + 4]" `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueNumber 3, HiValueNumber 7]))

  "[# (1 + 2) #]" `isEvaluatedTo` ParseError

  describe "Keys after dot" $ do
    "{\"\": 42}." `isEvaluatedTo` ParseError
    "{\">>=\": 42}.>>=" `isEvaluatedTo` ParseError

    "{\"aboba\": 42}.aboba" `isEvaluatedTo` Success (HiValueNumber 42)
    "{\"a-b-o-b-a\": 42}.a-b-o-b-a" `isEvaluatedTo` Success (HiValueNumber 42)

  describe "Rand doesn't need permissions" $ do
    it "no permissions" $ case unsafeRunHi [] "rand(0, 10)!" of
      Success _ -> return ()
      _         -> expectationFailure "Rand doesn't need permissions"
    it "all permissions" $ case unsafeRunHi allPerms "rand(0, 10)!" of
      Success _ -> return ()
      _         -> expectationFailure "Rand doesn't need permissions"

  "1 < 2 < 3" `isEvaluatedTo` ParseError

  describe "pack-bytes on arguments out of [0, 255]" $ do
    "pack-bytes([1, 2, 256])" `isEvaluatedTo` EvalError HiErrorInvalidArgument
    "pack-bytes([1, 2, -42])" `isEvaluatedTo` EvalError HiErrorInvalidArgument

  describe "\"cat\" * 0 and \"cat\" * -1 is invalid, positive value required" $ do
    "\"cat\" * 0"  `isEvaluatedTo` EvalError HiErrorInvalidArgument
    "\"cat\" * -1" `isEvaluatedTo` EvalError HiErrorInvalidArgument

  describe "dot access is not only for dictionaries" $ do
    "reverse.hello" `isEvaluatedTo` Success (HiValueString "olleh")

  "reverse.to-upper.hello" `isEvaluatedTo` EvalError HiErrorInvalidArgument

  describe "empty literals" $ do
    "[]"             `isEvaluatedTo` Success (HiValueList S.empty)
    "list()"         `isEvaluatedTo` Success (HiValueList S.empty)
    "[# #]"          `isEvaluatedTo` Success (HiValueBytes BS.empty)
    "pack-bytes([])" `isEvaluatedTo` Success (HiValueBytes BS.empty)
    "{}"             `isEvaluatedTo` Success (HiValueDict M.empty)

  "if(true, {\"width\" : 1}, 1+1).width" `isEvaluatedTo` Success (HiValueNumber 1)

  "{\"A\" : 1}.B" `isEvaluatedTo` Success HiValueNull

  -- it won't be checked but i've implemented it
  "fold([1,2,3], [1,3])" `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueNumber 2, HiValueNumber 3]))

  "\"suicide\"(4,100)" `isEvaluatedTo` Success (HiValueString "ide")

  "{}.add(1,1)" `hasSameParseTreeWith` "({}(\"add\"))(1,1)"

  "{1 : {2 : {3 : \"aboba\"}}}" `isEvaluatedTo`
    Success (HiValueDict $ M.fromList
      [(HiValueNumber 1, HiValueDict $ M.fromList
        [(HiValueNumber 2, HiValueDict $ M.fromList
          [(HiValueNumber 3, HiValueString "aboba")])])])

  it "now! - now! < 0" $ case unsafeRunHi allPerms "now! - now!" of
    Success (HiValueNumber val) -> val `shouldSatisfy` (< 0)
    _                           -> expectationFailure ""

  -- this will cause compilation error if wrong 'ByteString' was used
  let val = HiValueBytes BS.empty

  "serialise(1/0)" `isEvaluatedTo` EvalError HiErrorDivideByZero

  describe "python slices semantics" $ do
    "\"abc\"(0, 5)"      `isEvaluatedTo` Success (HiValueString "abc")
    "\"abc\"(3, 1)"      `isEvaluatedTo` Success (HiValueString "")
    "\"abcd\"(3, -3)"    `isEvaluatedTo` Success (HiValueString "")
    "\"abc\"(-10, null)" `isEvaluatedTo` Success (HiValueString "abc")

  "(div)(1,2)" `isEvaluatedTo` Success (HiValueNumber 0.5)

  "or(true, 3)" `isEvaluatedTo` Success (HiValueBool True)

  describe "rational in string replication" $ do
    "\"Cat\" * 5.2" `isEvaluatedTo` EvalError HiErrorInvalidArgument
    "\"Cat\" * 5.9" `isEvaluatedTo` EvalError HiErrorInvalidArgument
    -- "\"Cat\" * 5.2" `isEvaluatedTo` Success (HiValueString "CatCatCatCatCat")
    -- "\"Cat\" * 5.9" `isEvaluatedTo` Success (HiValueString "CatCatCatCatCat")

  describe "dot associativity" $ do
    "{}.a.b"            `hasSameParseTreeWith` "({}.a).b"
    "{}.a(\"hello\").b" `hasSameParseTreeWith` "(({}.a)(\"hello\")).b"
    "{}.a(\"hello\")!"  `hasSameParseTreeWith` "(({}.a)(\"hello\"))!"
    "{ \"a\" : { \"A\" : {\"X\" : [1, 2]} } }.a(\"A\").X(0)" `isEvaluatedTo` Success (HiValueNumber 1)

  describe "accessing sequnces" $ do
    "[1,2,3](0)"     `isEvaluatedTo` Success (HiValueNumber 1)
    "[# 00 ff #](1)" `isEvaluatedTo` Success (HiValueNumber 255)

  assertExecution' [(allPerms, "echo(\"hello\")!!", EvalError HiErrorInvalidArgument, "hello\n")]

  "{\"a boba\": 42}.a boba" `isEvaluatedTo` ParseError

  describe "boolean operator associativity" $ do
    "true && true && true" `hasSameParseTreeWith` "true && (true && true)"
    "true || true || true" `hasSameParseTreeWith` "true || (true || true)"

  describe "serialization of some values" $ do
    "deserialise(serialise(1))" `isEvaluatedTo` Success (HiValueNumber 1)

    forM_ ([minBound..maxBound] :: [HiFun]) $ \fun -> do
      ("deserialise(serialise(" ++ show (pretty fun) ++ "))") `isEvaluatedTo` Success (HiValueFunction fun)

    "deserialise(serialise(true))"           `isEvaluatedTo` Success (HiValueBool True)
    "deserialise(serialise(null))"           `isEvaluatedTo` Success HiValueNull
    "deserialise(serialise(\"aboba\"))"      `isEvaluatedTo` Success (HiValueString "aboba")
    "deserialise(serialise([1, 2, 3]))"      `isEvaluatedTo` Success (HiValueList (S.fromList [HiValueNumber 1, HiValueNumber 2, HiValueNumber 3]))
    "deserialise(serialise([# 01 02 03 #]))" `isEvaluatedTo` Success (HiValueBytes (BS.pack [1, 2, 3]))
    "deserialise(serialise(cwd))"            `isEvaluatedTo` Success (HiValueAction HiActionCwd)
    "deserialise(serialise(parse-time(\"2021-12-15 00:42:33.02949461 UTC\")))" `isEvaluatedTo` Success (HiValueTime (read "2021-12-15 00:42:33.02949461 UTC"))
    "deserialise(serialise({42:42}))"        `isEvaluatedTo` Success (HiValueDict (M.fromList [(HiValueNumber 42, HiValueNumber 42)]))

  "\"a\\\"b\"" `isPrettyPrintedAs` "\"a\\\"b\""

  describe "different byte literals" $ do
    "[#   00     #]" `isEvaluatedTo` Success (HiValueBytes (BS.pack [0]))
    "[# 00     ff#]" `isEvaluatedTo` Success (HiValueBytes (BS.pack [0, 255]))
    "[# 00 ff #]"    `isEvaluatedTo` Success (HiValueBytes (BS.pack [0, 255]))

  describe "bytes literal operations" $ do
    "length([# 00 ff #])"       `isEvaluatedTo` Success (HiValueNumber 2)
    "reverse([# 00 ff #])"      `isEvaluatedTo` Success (HiValueBytes (BS.pack [255, 0]))
    "[# 00 ff #] * 3"           `isEvaluatedTo` Success (HiValueBytes (BS.pack [0, 255, 0, 255, 0, 255]))
    "[# 00 ff #] + [# ff 00 #]" `isEvaluatedTo` Success (HiValueBytes (BS.pack [0, 255, 255, 0]))

  describe "fold" $ do
    "fold(add, [1, 2, 3, 4, 5])" `isEvaluatedTo` Success (HiValueNumber 15)
    "fold(sub, [1, 2, 3, 4, 5])" `isEvaluatedTo` Success (HiValueNumber (-13))
    "fold(mul, [1, 2, 3, 4, 5])" `isEvaluatedTo` Success (HiValueNumber 120)
    "fold(div, [1, 2, 3, 4, 5])" `isEvaluatedTo` Success (HiValueNumber (1 / 2 / 3 / 4 / 5))
    "fold(and, [true, true, true, true, false])"   `isEvaluatedTo` Success (HiValueBool False)
    "fold(or, [false, false, false, false, true])" `isEvaluatedTo` Success (HiValueBool True)

  "reverse." `isEvaluatedTo` ParseError

  -- todo: other tests if they will appear...

taskTests :: IO TestTree
taskTests = testSpec "Test from Q&A" taskTestSpec
