{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator (eval) where

import Control.Applicative (Applicative (liftA2), optional, (<|>))
import Control.Monad (foldM, forM, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State (execStateT, get, modify, put)

import qualified Codec.Compression.Zlib as Z
import Codec.Serialise (deserialise, serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Data.Foldable (Foldable (toList), fold)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (addUTCTime, diffUTCTime)

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..),
                 HiValueConvertable (..))

import Text.Read (readMaybe)


type HiExcept m = ExceptT HiError m

-- | Evaluates 'HiExpr'.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ evalExpr expr

-- | Evaluates 'HiExpr' in HiExcept.
evalExpr :: HiMonad m => HiExpr -> HiExcept m HiValue
evalExpr (HiExprValue argV)       = return argV
evalExpr (HiExprApply appE argsE) = do
    appV <- evalExpr appE
    case appV of
      (HiValueFunction fun) -> evalFun fun argsE
      (HiValueString text)  -> evalAccess text argsE
      (HiValueList seq)     -> evalAccess seq argsE
      (HiValueBytes bytes)  -> evalAccess bytes argsE
      (HiValueDict dict)    -> evalAccessDict dict argsE
      _                     -> throwE HiErrorInvalidFunction
evalExpr (HiExprRun argE)         = do
    argV <- evalExpr argE
    case argV of
      HiValueAction action -> lift $ runAction action
      _                    -> throwE HiErrorInvalidArgument
evalExpr (HiExprDict listDictE)   = do
    evaluatedDict <- forM listDictE $ \(leftE, rightE) -> do
      left  <- evalExpr leftE
      right <- evalExpr rightE
      return (left, right)
    return $ HiValueDict $ M.fromList evaluatedDict

-- | Evaluates function call.
evalFun :: HiMonad m => HiFun -> [HiExpr] -> HiExcept m HiValue
evalFun fun = case fun of
  HiFunAdd            -> evalFunAdd
  HiFunSub            -> evalFunSub
  HiFunMul            -> evalFunMul
  HiFunDiv            -> evalFunDiv
  HiFunNot            -> evalFunNot
  HiFunAnd            -> evalFunAnd
  HiFunOr             -> evalFunOr
  HiFunLessThan       -> evalFunLessThan
  HiFunGreaterThan    -> evalFunGreaterThan
  HiFunEquals         -> evalFunEquals
  HiFunNotLessThan    -> evalFunNotLessThan
  HiFunNotGreaterThan -> evalFunNotGreaterThan
  HiFunNotEquals      -> evalFunNotEquals
  HiFunIf             -> evalFunIf
  HiFunLength         -> evalFunLength
  HiFunToUpper        -> evalFunToUpper
  HiFunToLower        -> evalFunToLower
  HiFunReverse        -> evalFunReverse
  HiFunTrim           -> evalFunTrim
  HiFunList           -> evalFunList
  HiFunRange          -> evalFunRange
  HiFunFold           -> evalFunFold
  HiFunPackBytes      -> evalFunPackBytes
  HiFunUnpackBytes    -> evalFunUnpackBytes
  HiFunEncodeUtf8     -> evalFunEncodeUtf8
  HiFunDecodeUtf8     -> evalFunDecodeUtf8
  HiFunZip            -> evalFunZip
  HiFunUnzip          -> evalFunUnzip
  HiFunSerialise      -> evalFunSerialise
  HiFunDeserialise    -> evalFunDeserialise
  HiFunRead           -> evalFunRead
  HiFunWrite          -> evalFunWrite
  HiFunMkDir          -> evalFunMkDir
  HiFunChDir          -> evalFunChDir
  HiFunParseTime      -> evalFunParseTime
  HiFunRand           -> evalFunRand
  HiFunEcho           -> evalFunEcho
  HiFunCount          -> evalFunCount
  HiFunKeys           -> evalFunKeys
  HiFunValues         -> evalFunValues
  HiFunInvert         -> evalFunInvert

--------------------------------------------------------------------------------
---- Arithmetic operators
--------------------------------------------------------------------------------

-- | @add@ implementation.
evalFunAdd :: HiMonad m => [HiExpr] -> HiExcept m HiValue
evalFunAdd [argE1, argE2] = do
    argV1 <- evalExpr argE1
    argV2 <- evalExpr argE2
    case (argV1, argV2) of
      -- numbers
      (HiValueNumber arg1, HiValueNumber arg2)   -> return $ HiValueNumber $ arg1 + arg2

      -- strings
      (HiValueNull, HiValueNull)                 -> return HiValueNull
      (HiValueNull, HiValueString text)          -> return $ HiValueString text
      (HiValueString text, HiValueNull)          -> return $ HiValueString text
      (HiValueString arg1, HiValueString arg2)   -> return $ HiValueString $ arg1 <> arg2

      -- lists
      (HiValueNull, HiValueList seq)             -> return $ HiValueList seq
      (HiValueList seq, HiValueNull)             -> return $ HiValueList seq
      (HiValueList seq1, HiValueList seq2)       -> return $ HiValueList $ seq1 <> seq2

      -- bytes
      (HiValueNull, HiValueBytes bytes)          -> return $ HiValueBytes bytes
      (HiValueBytes bytes, HiValueNull)          -> return $ HiValueBytes bytes
      (HiValueBytes bytes1, HiValueBytes bytes2) -> return $ HiValueBytes $ bytes1 <> bytes2

      -- time
      (HiValueTime time, HiValueNumber arg)      -> return $ HiValueTime $ addUTCTime (realToFrac arg) time

      _                                          -> throwE HiErrorInvalidArgument
evalFunAdd _ = throwE HiErrorArityMismatch


-- | @sub@ implementation.
evalFunSub :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunSub [argE1, argE2] = do
    argV1 <- evalExpr argE1
    argV2 <- evalExpr argE2
    case (argV1, argV2) of
      -- numbers
      (HiValueNumber arg1, HiValueNumber arg2) -> return $ HiValueNumber $ arg1 - arg2

      -- time
      (HiValueTime time1, HiValueTime time2)   -> return $ HiValueNumber $ realToFrac $ diffUTCTime time1 time2

      _                                        -> throwE HiErrorInvalidArgument
evalFunSub _ = throwE HiErrorArityMismatch

-- | @div@ implementation.
evalFunDiv :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunDiv [argE1, argE2] = do
    argV1 <- evalExpr argE1
    argV2 <- evalExpr argE2
    case (argV1, argV2) of
      -- numbers
      (HiValueNumber val1, HiValueNumber val2)   -> do
        when (val2 == 0) $ throwE HiErrorDivideByZero
        return $ HiValueNumber $ val1 / val2

      -- strings (paths)
      (HiValueString text1, HiValueString text2) -> return $ HiValueString $ text1 <> "/" <> text2
      (HiValueNull, HiValueString text)          -> return $ HiValueString text
      (HiValueString text, HiValueNull)          -> return $ HiValueString text

      _                                          -> throwE HiErrorInvalidArgument
evalFunDiv _ = throwE HiErrorArityMismatch

-- | @mul@ implementation.
evalFunMul :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunMul [argE1, argE2] = do
    argV1 <- evalExpr argE1
    argV2 <- evalExpr argE2
    case (argV1, argV2) of
      -- numbers
      (HiValueNumber val1, HiValueNumber val2)  -> return $ HiValueNumber $ val1 * val2

      -- string * number
      (HiValueString text, HiValueNumber valR)               -> do
        valI <- expectInt valR
        unless (valI > 0) $ throwE HiErrorInvalidArgument
        return $ HiValueString $ valI `stimes` text
      (HiValueNull, HiValueNumber valR)                      -> do
        void $ expectInt valR
        return HiValueNull

      -- list * number
      (HiValueList seq, HiValueNumber valR)                  -> do
        valI <- expectInt valR
        unless (valI > 0) $ throwE HiErrorInvalidArgument
        return $ HiValueList $ valI `stimes` seq

      -- bytes
      (HiValueBytes bytes, HiValueNumber valR)               -> do
        valI <- expectInt valR
        unless (valI > 0) $ throwE HiErrorInvalidArgument
        return $ HiValueBytes $ valI `stimes` bytes

      _                                         -> throwE HiErrorInvalidArgument
evalFunMul _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Boolean operators
--------------------------------------------------------------------------------

-- | @not@ implementation.
evalFunNot :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunNot [argE] = do
    argV <- evalExpr argE
    case argV of
      HiValueBool arg -> return $ HiValueBool (not arg)
      _               -> throwE HiErrorInvalidArgument
evalFunNot _ = throwE HiErrorArityMismatch

-- | @and@ implementation.
evalFunAnd :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunAnd [argE1, argE2] = do
  argV1 <- evalExpr argE1
  case argV1 of
    HiValueBool False -> return argV1
    HiValueNull       -> return argV1
    _                 -> evalExpr argE2
evalFunAnd _ = throwE HiErrorArityMismatch

-- | @or@ implementation.
evalFunOr :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunOr  [argE1, argE2] = do
  argV1 <- evalExpr argE1
  case argV1 of
    HiValueBool False -> evalExpr argE2
    HiValueNull       -> evalExpr argE2
    _                 -> return argV1
evalFunOr _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Comparison
--------------------------------------------------------------------------------

-- | @less-than@ implementation.
evalFunLessThan :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunLessThan argsE       = HiValueBool <$> less argsE

-- | @greater-than@ implementation.
evalFunGreaterThan :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunGreaterThan argsE    = HiValueBool <$> liftA2 (&&) (not <$> less argsE) (not <$> equals argsE)

-- | @equals@ implementation.
evalFunEquals :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunEquals argsE         = HiValueBool <$> equals argsE

-- | @not-less-than@ implementation.
evalFunNotLessThan :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunNotLessThan argsE    = HiValueBool <$> (not <$> less argsE)

-- | @not-greater-than@ implementation.
evalFunNotGreaterThan :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunNotGreaterThan argsE = HiValueBool <$> liftA2 (||) (less argsE) (equals argsE)

-- | @not-equals@ implementation.
evalFunNotEquals :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunNotEquals argsE      = HiValueBool <$> (not <$> equals argsE)

--------------------------------------------------------------------------------
---- if-else
--------------------------------------------------------------------------------

-- | @if@ implementation.
evalFunIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunIf [argE1, argE2, argE3] = do
  argV1 <- evalExpr argE1
  case argV1 of
    HiValueBool True  -> evalExpr argE2
    HiValueBool False -> evalExpr argE3
    _                 -> throwE HiErrorInvalidArgument
evalFunIf _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Sequences
--------------------------------------------------------------------------------

-- | @length@ implementation.
evalFunLength :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunLength [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueNull        -> return $ toHiValue (0 :: Int)
    HiValueString text -> return $ toHiValue $ T.length text
    HiValueList seq    -> return $ toHiValue $ S.length seq
    HiValueBytes bytes -> return $ toHiValue $ BS.length bytes
    _                  -> throwE HiErrorInvalidArgument
evalFunLength _ = throwE HiErrorArityMismatch

-- | @to-upper@ implementation.
evalFunToUpper :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunToUpper [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueNull        -> return HiValueNull
    HiValueString text -> return $ HiValueString $ T.toUpper text
    _                  -> throwE HiErrorInvalidArgument
evalFunToUpper _ = throwE HiErrorArityMismatch

-- | @to-lower@ implementation.
evalFunToLower :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunToLower [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueNull        -> return HiValueNull
    HiValueString text -> return $ HiValueString $ T.toLower text
    _                  -> throwE HiErrorInvalidArgument
evalFunToLower _ = throwE HiErrorArityMismatch

-- | @reverse@ implementation.
evalFunReverse :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunReverse [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueNull        -> return HiValueNull
    HiValueString text -> return $ HiValueString $ T.reverse text
    HiValueList seq    -> return $ HiValueList $ S.reverse seq
    HiValueBytes bytes -> return $ HiValueBytes $ BS.reverse bytes
    _                  -> throwE HiErrorInvalidArgument
evalFunReverse _ = throwE HiErrorArityMismatch

-- | @trim@ implementation.
evalFunTrim :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunTrim [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueNull        -> return HiValueNull
    HiValueString text -> return $ HiValueString $ T.strip text
    _                  -> throwE HiErrorInvalidArgument
evalFunTrim _ = throwE HiErrorArityMismatch

-- | @list@ implementation.
evalFunList :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunList argsE = do
  argsV <- forM argsE evalExpr
  return $ HiValueList $ S.fromList argsV

-- | @range@ implementation.
evalFunRange :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunRange [argE1, argE2] = do
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2
  case (argV1, argV2) of
    (HiValueNumber arg1, HiValueNumber arg2) -> return $ HiValueList $ S.fromList $ toHiValue <$> [arg1..arg2]
    _                                        -> throwE HiErrorInvalidArgument
evalFunRange _ = throwE HiErrorArityMismatch

-- | @fold@ implementation.
evalFunFold :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunFold [argE1, argE2] = do
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2
  case argV2 of
    HiValueList seqV -> do
      let listV = toList seqV
      let listE = HiExprValue <$> listV
      let foldFun = \v1 v2 -> evalExpr $ HiExprApply (HiExprValue argV1) (HiExprValue <$> [v1, v2])
      case listV of
        []   -> return HiValueNull
        v:vs -> foldM foldFun v vs
    _               -> throwE HiErrorInvalidArgument
evalFunFold _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Bytes
--------------------------------------------------------------------------------

-- | @pack-bytes@ implementation.
evalFunPackBytes :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunPackBytes [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueList seq -> do
      seqWord8 <- forM seq $ \case
        HiValueNumber argR -> do
          argI <- expectInt argR
          unless (0 <= argI && argI <= 255) $ throwE HiErrorInvalidArgument
          return $ fromIntegral argI
        _                  -> throwE HiErrorInvalidArgument
      let bytes = BS.pack $ toList seqWord8
      return $ HiValueBytes bytes
    _               -> throwE HiErrorInvalidArgument
evalFunPackBytes _ = throwE HiErrorArityMismatch

-- | @unpack-bytes@ implementation.
evalFunUnpackBytes :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunUnpackBytes [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueBytes bytes -> do
      let seqWord8 = S.fromList $ toHiValue <$> BS.unpack bytes
      return $ HiValueList seqWord8
    _                  -> throwE HiErrorInvalidArgument
evalFunUnpackBytes _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- UTF-8
--------------------------------------------------------------------------------

-- | @encode-utf8@ implementation.
evalFunEncodeUtf8 :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunEncodeUtf8 [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueString text -> do
      let encoded = BL.toStrict $ UTF8.fromString $ T.unpack text
      return $ HiValueBytes encoded
    _                  -> throwE HiErrorInvalidArgument
evalFunEncodeUtf8 _ = throwE HiErrorArityMismatch

-- | @decode-utf8@ implementation.
evalFunDecodeUtf8 :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunDecodeUtf8 [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueBytes bytes -> do
      case decodeUtf8' bytes of
        Left _        -> return HiValueNull
        Right decoded -> return $ HiValueString decoded
    _                  -> throwE HiErrorInvalidArgument
evalFunDecodeUtf8 _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Zip
--------------------------------------------------------------------------------

-- | @zip@ implementation.
evalFunZip :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunZip [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueBytes bytes -> return $ HiValueBytes $ useStrict compressBytes bytes
    _                  -> throwE HiErrorInvalidArgument
  where
    compressBytes = Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.bestCompression }
evalFunZip _ = throwE HiErrorArityMismatch

-- | @unzip@ implementation.
evalFunUnzip :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunUnzip [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueBytes bytes -> return $ HiValueBytes $ useStrict Z.decompress bytes
    _                  -> throwE HiErrorInvalidArgument
evalFunUnzip _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Serialisation
--------------------------------------------------------------------------------

-- | @serialise@ implementation.
evalFunSerialise :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunSerialise [argE] = do
  argV <- evalExpr argE
  return $ HiValueBytes $ BL.toStrict $ serialise argV
evalFunSerialise _ = throwE HiErrorArityMismatch

-- | @deserialise@ implementation.
evalFunDeserialise :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunDeserialise [argE] = do
  argV <- evalExpr argE
  case argV of
    HiValueBytes bytes -> return $ deserialise $ BL.fromStrict bytes
    _                  -> throwE HiErrorInvalidArgument
evalFunDeserialise _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- IO
--------------------------------------------------------------------------------

-- | @read@ implementation.
evalFunRead :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunRead [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueString path) -> return $ HiValueAction $ HiActionRead $ T.unpack path
    _                    -> throwE HiErrorInvalidArgument
evalFunRead _ = throwE HiErrorArityMismatch

-- | @write@ implementation.
evalFunWrite :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunWrite [argE1, argE2] = do
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2
  case (argV1, argV2) of
    (HiValueString path, HiValueString text) -> return $ HiValueAction $ HiActionWrite (T.unpack path) (BL.toStrict $ UTF8.fromString $ T.unpack text)
    _                                        -> throwE HiErrorInvalidArgument
evalFunWrite _ = throwE HiErrorArityMismatch

-- | @mkdir@ implementation.
evalFunMkDir :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunMkDir [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueString path) -> return $ HiValueAction $ HiActionMkDir $ T.unpack path
    _                    -> throwE HiErrorInvalidArgument
evalFunMkDir _ = throwE HiErrorArityMismatch

-- | @cd@ implementation.
evalFunChDir :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunChDir [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueString path) -> return $ HiValueAction $ HiActionChDir $ T.unpack path
    _                    -> throwE HiErrorInvalidArgument
evalFunChDir _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Time
--------------------------------------------------------------------------------

-- | @parse-time@ implementation.
evalFunParseTime :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunParseTime [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueString timeStr) -> do
      let maybeTime = readMaybe (T.unpack timeStr)
      case maybeTime of
        Just time -> return $ HiValueTime time
        _         -> throwE HiErrorInvalidArgument
    _                       -> throwE HiErrorInvalidArgument
evalFunParseTime _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Random
--------------------------------------------------------------------------------

-- | @rand@ implementation.
evalFunRand :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunRand [argE1, argE2] = do
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2
  case (argV1, argV2) of
    (HiValueNumber fromR, HiValueNumber toR) -> do
      from <- expectInt fromR
      to   <- expectInt toR
      unless (from <= to) $ throwE HiErrorInvalidArgument
      return $ HiValueAction $ HiActionRand from to
    _                                        -> throwE HiErrorInvalidArgument
evalFunRand _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Echo
--------------------------------------------------------------------------------

-- | @echo@ implementation.
evalFunEcho :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunEcho [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueString msg) -> return $ HiValueAction $ HiActionEcho msg
    _                   -> throwE HiErrorInvalidArgument
evalFunEcho _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Dictionary
--------------------------------------------------------------------------------

-- | @keys@ implementation.
evalFunKeys :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunKeys [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueDict dict) -> return $ toHiValue $ M.keys dict
    _                  -> throwE HiErrorInvalidArgument
evalFunKeys _ = throwE HiErrorArityMismatch

-- | @values@ implementation.
evalFunValues :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunValues [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueDict dict) -> return $ toHiValue $ M.elems dict
    _                  -> throwE HiErrorInvalidArgument
evalFunValues _ = throwE HiErrorArityMismatch

-- | @count@ implementation.
evalFunCount :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunCount [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueList seq)    -> return $ toHiValue $ M.fromList $ countOccurencesList (toList seq)
    (HiValueString text) -> return $ toHiValue $ M.fromList $ countOccurencesList (T.unpack text)
    (HiValueBytes bytes) -> return $ toHiValue $ M.fromList $ countOccurencesList (BS.unpack bytes)
    _                    -> throwE HiErrorInvalidArgument
  where
    countOccurencesList list = [(val, count val list) | val <- L.nub list]
    count e list = L.length $ L.filter (== e) list
evalFunCount _ = throwE HiErrorArityMismatch

-- | @invert@ implementation.
evalFunInvert :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunInvert [argE] = do
  argV <- evalExpr argE
  case argV of
    (HiValueDict dict) -> return $ toHiValue $ invert dict
    _                  -> throwE HiErrorInvalidArgument
  where
    invert = M.foldlWithKey (\m k v -> M.alter (put k) v m) M.empty
    put a (Just as) = Just (a:as)
    put a Nothing   = Just [a]
evalFunInvert _ = throwE HiErrorArityMismatch

--------------------------------------------------------------------------------
---- Sequances accessing
--------------------------------------------------------------------------------

-- | Represents set of types which can provide operations of getting element by index and getting of slice.
class Access a where
  -- | Accessing by index to @a@.
  index :: a -> Int -> HiValue

  -- | Getting of slice from @a@ by specified indexes.
  slice :: a -> Int -> Int -> HiValue

  -- | Number of elements in @a@.
  alength :: a -> Int

-- | Evaluates result of accessing to accessable object.
evalAccess :: (HiMonad m, Access a, HiValueConvertable a) => a -> [HiExpr] -> HiExcept m HiValue
evalAccess a [argE] = do -- accessing by index
  argV <- evalExpr argE
  case argV of
    HiValueNumber posR -> do
      posI <- expectInt posR
      if 0 <= posI && posI < alength a
        then return $ toHiValue $ index a posI
        else return HiValueNull
    _                 -> throwE HiErrorInvalidArgument
evalAccess a [argE1, argE2] = do -- getting slice
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2

  -- retrives index from HiValue
  let retrieveIdxOrIfNull arg ifNull = case arg of
        HiValueNumber pos -> expectInt pos
        HiValueNull       -> return ifNull
        _                 -> throwE HiErrorInvalidArgument

  -- normalizes index
  let normIdx = do
        idx <- get
        when (idx < - alength a) $ put 0
        when (- alength a <= idx && idx < 0) $ modify (+ alength a)
        when (idx > alength a) $ put (alength a)

  -- evaluates result of slice getting
  let evalSlice = do
        from <- argV1 `retrieveIdxOrIfNull` 0
        to   <- argV2 `retrieveIdxOrIfNull` alength a
        normedFrom <- execStateT normIdx from
        normedTo   <- execStateT normIdx to
        return $ slice a normedFrom normedTo

  evalSlice
evalAccess _ _ = throwE HiErrorArityMismatch

instance Access T.Text where
  index text pos = toHiValue $ T.pack [T.index text pos]
  slice text from to = toHiValue $ T.take (to - from) . T.drop from $ text
  alength = T.length

instance HiValueConvertable a => Access (S.Seq a) where
  index text pos = toHiValue $ S.index text pos
  slice text from to = toHiValue $ S.take (to - from) . S.drop from $ text
  alength = S.length

instance Access BS.ByteString where
  index text pos = toHiValue $ BS.index text pos
  slice text from to = toHiValue $ BS.take (to - from) . BS.drop from $ text
  alength = BS.length

--------------------------------------------------------------------------------
---- Dictionary accessing
--------------------------------------------------------------------------------

-- | Evaluates result of dict accessing.
evalAccessDict :: HiMonad m => M.Map HiValue HiValue -> [HiExpr] -> HiExcept m HiValue
evalAccessDict dict [argE] = do
  key <- evalExpr argE
  case M.lookup key dict of
    Just val -> return val
    Nothing  -> return HiValueNull
evalAccessDict _ _ = throwE HiErrorInvalidArgument

--------------------------------------------------------------------------------
---- Comparison implementation
--------------------------------------------------------------------------------

-- | Checks equality of expressions. Fails if number of argument isn't equal to 2.
equals :: HiMonad m => [HiExpr] -> HiExcept m Bool
equals [argE1, argE2] = do
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2
  return $ argV1 == argV2
equals _ = throwE HiErrorInvalidArgument

-- | Checks if first exprssion in list is less than second. Fails if number of argument isn't equal to 2.
less :: HiMonad m => [HiExpr] -> HiExcept m Bool
less [argE1, argE2] = do
  argV1 <- evalExpr argE1
  argV2 <- evalExpr argE2
  return $ case (argV1, argV2) of
    (HiValueBool _, HiValueNumber _) -> True
    (HiValueNumber _, HiValueBool _) -> False
    (argV1, argV2)                   -> argV1 < argV2
less _ = throwE HiErrorInvalidArgument

--------------------------------------------------------------------------------
---- Utilities
--------------------------------------------------------------------------------

-- | Casts 'Rational' to 'Int'. Fails with 'HiErrorInvalidArgument' if argument doesn't represent integer value.
expectInt :: HiMonad m => Rational -> HiExcept m Int
expectInt val = do
  let (q, r) = quotRem (numerator val) (denominator val)
  if r == 0
    then do
      let intMinBound = fromIntegral (minBound :: Int)
      let intMaxBound = fromIntegral (maxBound :: Int)
      unless (intMinBound <= q && q <= intMaxBound) $ throwE HiErrorInvalidArgument
      return $ fromIntegral q
    else throwE HiErrorInvalidArgument

-- | Converts function transforming lazy ByteString to function transforming strict ByteString.
useStrict :: (BL.ByteString -> BL.ByteString) -> (BS.ByteString -> BS.ByteString)
useStrict f = BL.toStrict . f . BL.fromStrict
