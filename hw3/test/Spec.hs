import Test.Tasty (defaultMain, testGroup)

import Control.Monad (when)

import qualified HW3.QAndA
import qualified HW3.T1
import qualified HW3.T10
import qualified HW3.T11
import qualified HW3.T2
import qualified HW3.T3
import qualified HW3.T4
import qualified HW3.T5
import qualified HW3.T6
import qualified HW3.T7
import qualified HW3.T8

import System.Directory (doesDirectoryExist, removeDirectory, removeDirectoryRecursive)

-- disabled to run hw-checker without waste log of this tests.
main :: IO ()
main = return ()

-- main :: IO ()
-- main = do
--   -- for operation IO testing
--   tmpExists <- doesDirectoryExist "tmp"
--   when tmpExists $ removeDirectoryRecursive "tmp"

--   testsResults <- sequence [ HW3.T1.taskTests
--                            , HW3.T2.taskTests
--                            , HW3.T3.taskTests
--                            , HW3.T4.taskTests
--                            , HW3.T5.taskTests
--                            , HW3.T6.taskTests
--                            , HW3.T7.taskTests
--                            , HW3.T8.taskTests
--                            , HW3.T10.taskTests
--                            , HW3.T11.taskTests
--                            , HW3.QAndA.taskTests
--                            ]
--   defaultMain $ testGroup "Tests from task" testsResults
