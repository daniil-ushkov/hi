{-# LANGUAGE OverloadedStrings #-}

module HW3.T6 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (Success), isEvaluatedTo, isParsedTo, isPrettyPrintedAs)

import qualified Data.ByteString as BS
import qualified Data.Sequence as S

taskTestSpec :: Spec
taskTestSpec = do
  describe "identifiers parsing" $ do
    "pack-bytes"   `isParsedTo` HiExprValue (HiValueFunction HiFunPackBytes)
    "unpack-bytes" `isParsedTo` HiExprValue (HiValueFunction HiFunUnpackBytes)
    "encode-utf8"  `isParsedTo` HiExprValue (HiValueFunction HiFunEncodeUtf8)
    "decode-utf8"  `isParsedTo` HiExprValue (HiValueFunction HiFunDecodeUtf8)
    "zip"          `isParsedTo` HiExprValue (HiValueFunction HiFunZip)
    "unzip"        `isParsedTo` HiExprValue (HiValueFunction HiFunUnzip)
    "serialise"    `isParsedTo` HiExprValue (HiValueFunction HiFunSerialise)
    "deserialise"  `isParsedTo` HiExprValue (HiValueFunction HiFunDeserialise)

  describe "bytes literal" $ do
    "[# 01 3f ec #]" `isParsedTo` HiExprValue (HiValueBytes (BS.pack [1, 63, 236]))

  describe "evaluation" $ do
    "pack-bytes([ 3, 255, 158, 32 ])"   `isEvaluatedTo` Success (HiValueBytes (BS.pack [3, 255, 158, 32]))
    "unpack-bytes([# 10 20 30 #])"      `isEvaluatedTo` Success (HiValueList (S.fromList $ HiValueNumber <$> [16, 32, 48]))
    "encode-utf8(\"Hello!\")"           `isEvaluatedTo` Success (HiValueBytes (BS.pack [72, 101, 108, 108, 111, 33]))
    "decode-utf8([# 48 65 6c 6c 6f #])" `isEvaluatedTo` Success (HiValueString "Hello")
    "decode-utf8([# c3 28 #])"          `isEvaluatedTo` Success HiValueNull

    "unzip(zip([# 10 20 30 #]))" `isEvaluatedTo` Success (HiValueBytes (BS.pack [16, 32, 48]))
    "unzip(zip([# 04 02 #]))"    `isEvaluatedTo` Success (HiValueBytes (BS.pack [4, 2]))

    "deserialise(serialise(42))"        `isEvaluatedTo` Success (HiValueNumber 42)
    "deserialise(serialise(\"aboba\"))" `isEvaluatedTo` Success (HiValueString "aboba")
    "deserialise(serialise([1,2,3]))"   `isEvaluatedTo` Success (HiValueList (S.fromList $ HiValueNumber <$> [1, 2, 3]))
    "deserialise(serialise(null))"      `isEvaluatedTo` Success HiValueNull

  describe "overload operators" $ do
    "[# 00 ff #] + [# 01 e3 #]" `isEvaluatedTo` Success (HiValueBytes (BS.pack [0, 255, 1, 227]))
    "[# 00 ff #] * 3"           `isEvaluatedTo` Success (HiValueBytes (BS.pack [0, 255, 0, 255, 0, 255]))

  describe "from REPL session" $ do
    "pack-bytes(range(30, 40))" `isPrettyPrintedAs` "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
    "zip(encode-utf8(\"Hello, World!\" * 1000))" `isPrettyPrintedAs` ("[# 78\n da\n ed\n c7\n 31\n 0d\n 00\n 20\n 0c\n 00\n 30\n 2b\n f0\n 23\n 64\n 0e\n 30\n 00\n df\n 92\n 25\n f3\n 7f\n a0\n 82\n af\n " ++
      "fd\n 1a\n 37\n b3\n d6\n d8\n d5\n 79\n 66\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n " ++
      "88\n 88\n 88\n 88\n 88\n 88\n 88\n 88\n fc\n c9\n 03\n ca\n 0f\n 3b\n 28 #]")
    "decode-utf8([# 68 69 #] * 5)" `isPrettyPrintedAs` "\"hihihihihi\""
    "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" `isPrettyPrintedAs` "[# 01 02 03 #]"

    describe "from Slack" $ do
      "[# 00 #](0)" `isEvaluatedTo` Success (HiValueNumber 0)

taskTests :: IO TestTree
taskTests = testSpec "Task 6: Bytes and serialisation" taskTestSpec
