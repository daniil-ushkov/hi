{-# LANGUAGE OverloadedStrings #-}

module HW3.T7 where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, testSpec)

import HW3.Action (HiPermission (AllowRead, AllowWrite), PermissionException (PermissionRequired))
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Utils (ParseAndEvalResult (AllowError, Success), allPerms, assertExecution,
                  isEvaluatedTo, isPrettyPrintedAs)

import qualified Data.ByteString as BS
import qualified Data.Sequence as S

import Control.Exception (try)
import Control.Monad (forM, when)

import Prettyprinter (pretty)

import System.Directory (doesDirectoryExist, removeDirectory, removeDirectoryRecursive)
import System.IO.Unsafe (unsafePerformIO)

import Text.Megaparsec.Error.Builder (err)

taskTestSpec :: Spec
taskTestSpec = do
  describe "not executed actions parsing" $ do
    "read(\"aboba.txt\")" `isEvaluatedTo` Success (HiValueAction (HiActionRead "aboba.txt"))
    "read(\"aboba.txt\")" `isPrettyPrintedAs` "read(\"aboba.txt\")"

    -- "write(\"aboba.txt\", [# 04 02 #])" `isEvaluatedTo` Success (HiValueAction (HiActionWrite "aboba.txt" (BS.pack [4, 2])))
    -- "write(\"aboba.txt\", [# 04 02 #])" `isPrettyPrintedAs` "write(\"aboba.txt\", [# 04 02 #])"
    "write(\"hi.txt\", \"Hi!\")" `isPrettyPrintedAs` "write(\"hi.txt\", [# 48 69 21 #])"

    "mkdir(\"aboba.txt\")" `isEvaluatedTo` Success (HiValueAction (HiActionMkDir "aboba.txt"))
    "mkdir(\"aboba.txt\")" `isPrettyPrintedAs` "mkdir(\"aboba.txt\")"

    "cd(\"aboba.txt\")" `isEvaluatedTo` Success (HiValueAction (HiActionChDir "aboba.txt"))
    "cd(\"aboba.txt\")" `isPrettyPrintedAs` "cd(\"aboba.txt\")"

    "cwd" `isEvaluatedTo` Success (HiValueAction HiActionCwd)
    "cwd" `isPrettyPrintedAs` "cwd"

  assertExecution
    [ (allPerms, "mkdir(\"tmp\")!",                    Success HiValueNull)
    , (allPerms, "read(\"tmp\")!",                     Success (HiValueList S.empty))
    , (allPerms, "mkdir(\"tmp/a\")!",                  Success HiValueNull)
    , (allPerms, "mkdir(\"tmp/b\")!",                  Success HiValueNull)
    , (allPerms, "read(\"tmp\")!",                     Success (HiValueList (S.fromList [HiValueString "a", HiValueString "b"])))
    , (allPerms, "write(\"tmp/hi.txt\", \"Hello\")!",  Success HiValueNull)
    , (allPerms, "cd(\"tmp\")!",                       Success HiValueNull)
    , (allPerms, "read(\"hi.txt\")!",                  Success (HiValueString "Hello"))

    , (allPerms, "read",                               Success (HiValueFunction HiFunRead))
    , (allPerms, "read(\"hi.txt\")",                   Success (HiValueAction (HiActionRead "hi.txt")))
    , (allPerms, "read(\"hi.txt\")!",                  Success (HiValueString "Hello"))

    , ([],       "cwd!",                               AllowError AllowRead)
    , ([],       "cd(\".\")!",                         AllowError AllowRead)
    , ([],       "read(\"aboba.txt\")!",               AllowError AllowRead)
    , ([],       "write(\"aboba.txt\", \"aboba\")!",   AllowError AllowWrite)
    , ([],       "mkdir(\"aboba\")!",                  AllowError AllowWrite)
    ]

taskTests :: IO TestTree
taskTests = testSpec "Task 7: File I/O" taskTestSpec
