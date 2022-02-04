module Main where

import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

import HW3.Action (HIO (runHIO), HiPermission (AllowRead, AllowTime, AllowWrite))
import HW3.Base (HiError)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)

import Control.Monad.Trans.Class (lift)

import Text.Megaparsec (errorBundlePretty)

import Data.Set (empty, fromList)

main :: IO ()
main = runInputT defaultSettings loop
   where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "hi> "
      case input of
        Nothing     -> loop
        Just input  -> do
          case parse input of
            Left err   -> outputStrLn $ errorBundlePretty err
            Right expr -> do
              evaluated <- lift $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              case evaluated of
                Left err   -> outputStrLn $ show err
                Right expr -> outputStrLn $ show (prettyValue expr)
          loop
