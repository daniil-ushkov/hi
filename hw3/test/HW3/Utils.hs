module HW3.Utils where

import HW3.Action (HIO (runHIO), HiPermission, PermissionException (PermissionRequired))
import HW3.Base (HiError, HiExpr, HiValue)
import HW3.Evaluator (eval)
import HW3.Parser (parse)

import qualified Prettyprinter as PP

import Test.Tasty.Hspec (Spec, SpecWith, it, runIO, shouldBe)

import Control.Exception (catch)
import Control.Monad (forM)

import qualified Data.Set as Set

import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import System.Directory (getCurrentDirectory, removeFile, setCurrentDirectory)
import System.IO (hClose, openTempFile, stdout)
import System.IO.Unsafe (unsafePerformIO)

-- | Represents result of evaluating for testing
data ParseAndEvalResult
  = ParseError
  | EvalError HiError
  | AllowError HiPermission
  | Success HiValue
  deriving (Show, Eq)

-- | Checks if 'input' is parsed in expected parse tree.
isParsedTo :: String -> HiExpr -> SpecWith ()
isParsedTo identifier expr = it testName $ parse identifier `shouldBe` Right expr
  where
    testName = "\"" ++ identifier ++ "\"" ++ " parsed to " ++ show expr

-- | List of all existing permissions.
allPerms :: [HiPermission]
allPerms = [minBound..maxBound]

-- | Runs @input@ with @perms@ unsafely.
unsafeRunHi :: [HiPermission] -> String -> ParseAndEvalResult
unsafeRunHi perms input = unsafePerformIO $ runHi perms input

-- | Runs @input@ with @perms@.
runHi :: [HiPermission] -> String -> IO ParseAndEvalResult
runHi perms input = case parse input of
  Left _     -> return ParseError
  Right expr -> do runResult <- runHIO (eval expr) (Set.fromList perms)
                   case runResult of
                     Left err  -> return $ EvalError err
                     Right val -> return $ Success val
                `catch` \(PermissionRequired perm) -> return $ AllowError perm

-- | Prints result as interpreter.
prettyprint :: ParseAndEvalResult -> String
prettyprint (Success val)   = show $ PP.pretty val
prettyprint (EvalError err) = show err
prettyprint result          = show result

-- | Checks if 'input' evaluates to 'result'.
isEvaluatedTo :: String -> ParseAndEvalResult -> SpecWith ()
isEvaluatedTo input result = it testName $ unsafeRunHi allPerms input `shouldBe` result
  where
    testName = input ++ " is evaluated to " ++ show (prettyprint result)

-- | Checks if interpreter prints 'output' on 'input'.
isPrettyPrintedAs :: String -> String -> SpecWith ()
isPrettyPrintedAs input output = it testName $ prettyprint (unsafeRunHi allPerms input) `shouldBe` output
  where
    testName = "hi> " ++ input ++ " | " ++ output

-- | Checks if inputs have same parse trees after parser running.
hasSameParseTreeWith :: String -> String -> SpecWith ()
hasSameParseTreeWith lhs rhs = it testName $ parse lhs `shouldBe` parse rhs
  where
    testName = lhs ++ " has same parse tree as " ++ rhs

type Perms = [HiPermission]

type HiIn = String

type Returns = ParseAndEvalResult

type Logs = String

-- | Checks if execution of specified commands has specified return values.
assertExecution :: [(Perms, HiIn, Returns)] -> Spec
assertExecution exectuion = do
  returnsAndLogs <- runIO $ returningToInitialDirectory $ do
    forM exectuion $ \(perms, hiIn, _) -> runHi perms hiIn

  it "execution is valid" $ do
    returnsAndLogs `shouldBe` ((\(_, _, returns) -> returns) <$> exectuion)

-- | Checks if execution of specified commands has specified return values and outputs to stdout.
assertExecution' :: [(Perms, HiIn, Returns, Logs)] -> Spec
assertExecution' exectuion = do
  returnsAndLogs <- runIO $ returningToInitialDirectory $ do
    forM exectuion $ \(perms, hiIn, _, _) -> capturingStdout $ runHi perms hiIn

  it "execution is valid" $ do
    returnsAndLogs `shouldBe` ((\(_, _, returns, logs) -> (returns, logs)) <$> exectuion)

-- | Wrapper for IO, which saves CWD before running evaluation and returns to it after.
returningToInitialDirectory :: IO a -> IO a
returningToInitialDirectory io = do
  cwd <- getCurrentDirectory
  result <- io
  setCurrentDirectory cwd
  return result

-- | Wrapper for IO, which captures output to stdout during evaluation.
capturingStdout :: IO a -> IO (a, String)
capturingStdout io = do
  (tmpPath, tmpHdl) <- openTempFile "." "test_stdout"
  stdoutDup <- hDuplicate stdout
  hDuplicateTo tmpHdl stdout

  result <- io

  hClose tmpHdl

  hDuplicateTo stdoutDup stdout

  capturedLog <- readFile tmpPath
  removeFile tmpPath

  return (result, capturedLog)
