module HW3.Action (HiPermission(..), PermissionException(..), HIO(..)) where

import Control.Exception (Exception, throwIO)
import Control.Monad (ap, liftM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random (randomRIO)

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..),
                 HiValueConvertable (..))

--------------------------------------------------------------------------------
---- Permissions
--------------------------------------------------------------------------------

-- | Represents permissions on actions.
data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
---- Exceptions
--------------------------------------------------------------------------------

-- | Exception which can be thrown by running of forbidden action.
data PermissionException = PermissionRequired HiPermission deriving Show

instance Exception PermissionException

--------------------------------------------------------------------------------
---- HIO
--------------------------------------------------------------------------------

-- | Implementation of HiMonad.
newtype HIO a = HIO { runHIO :: Set.Set HiPermission -> IO a }

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Monad HIO where
  return x = HIO{runHIO= \_ -> pure x}
  (>>=) HIO{runHIO=run} f = HIO{runHIO= \perm -> run perm >>= (\hio -> runHIO hio perm) . f}

instance MonadIO HIO where
  liftIO io = HIO{runHIO=const io}

-- | Checks does passed permission exist in permissions set.
isAllowed :: HiPermission -> HIO Bool
isAllowed perm = HIO{runHIO= \perms -> return $ Set.member perm perms}

instance HiMonad HIO where
  runAction (HiActionRead path)        = do
    allowed <- isAllowed AllowRead
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowRead)

    dirExists <- liftIO $ doesDirectoryExist path
    fileExists <- liftIO $ doesFileExist path
    if dirExists
      then do
        list <- liftIO $ listDirectory path
        return $ HiValueList $ HiValueString . T.pack <$> S.fromList list
      else if fileExists
        then do
          content <- liftIO $ BS.readFile path
          case decodeUtf8' content of
            Left _        -> return $ HiValueBytes content
            Right decoded -> return $ HiValueString decoded
        else return HiValueNull

  runAction (HiActionWrite path bytes) = do
    allowed <- isAllowed AllowWrite
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowWrite)

    liftIO $ BS.writeFile path bytes
    return HiValueNull

  runAction (HiActionMkDir path)       = do
    allowed <- isAllowed AllowWrite
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowWrite)
    liftIO $ createDirectory path
    return HiValueNull

  runAction (HiActionChDir path)       = do
    allowed <- isAllowed AllowRead
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowRead)
    liftIO $ setCurrentDirectory path
    return HiValueNull

  runAction HiActionCwd                = do
    allowed <- isAllowed AllowRead
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowRead)
    cwd <- liftIO getCurrentDirectory
    return $ HiValueString $ T.pack cwd

  runAction HiActionNow                = do
    allowed <- isAllowed AllowTime
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowTime)
    time <- liftIO getCurrentTime
    return $ HiValueTime time

  runAction (HiActionRand from to)     = do
    val <- liftIO $ randomRIO (from, to)
    return $ HiValueNumber $ toRational val

  runAction (HiActionEcho msg)         = do
    allowed <- isAllowed AllowWrite
    unless allowed $ liftIO $ throwIO (PermissionRequired AllowWrite)

    liftIO $ putStrLn $ T.unpack msg
    return HiValueNull
