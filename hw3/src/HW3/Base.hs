{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Base (HiFun(..), HiValue(..), HiValueConvertable(..), HiExpr(..), HiError(..), HiAction(..), HiMonad(..)) where

import qualified Data.ByteString as BS
import Data.Foldable (Foldable (fold), toList)
import qualified Data.List as List
import qualified Data.Map as M
import Data.Ratio (Ratio, denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Word (Word8)

import Codec.Serialise.Class (Serialise (..))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)

import qualified Prettyprinter as PP

import Text.Printf (printf)

import GHC.Generics (Generic)

--------------------------------------------------------------------------------
---- HiFun
--------------------------------------------------------------------------------

-- | HiFun represents functions of Hi language.
data HiFun
  = HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Serialise)

instance PP.Pretty HiFun where
  pretty HiFunAdd            = "add"
  pretty HiFunSub            = "sub"
  pretty HiFunMul            = "mul"
  pretty HiFunDiv            = "div"
  pretty HiFunNot            = "not"
  pretty HiFunAnd            = "and"
  pretty HiFunOr             = "or"
  pretty HiFunLessThan       = "less-than"
  pretty HiFunGreaterThan    = "greater-than"
  pretty HiFunEquals         = "equals"
  pretty HiFunNotLessThan    = "not-less-than"
  pretty HiFunNotGreaterThan = "not-greater-than"
  pretty HiFunNotEquals      = "not-equals"
  pretty HiFunIf             = "if"
  pretty HiFunLength         = "length"
  pretty HiFunToUpper        = "to-upper"
  pretty HiFunToLower        = "to-lower"
  pretty HiFunReverse        = "reverse"
  pretty HiFunTrim           = "trim"
  pretty HiFunList           = "list"
  pretty HiFunRange          = "range"
  pretty HiFunFold           = "fold"
  pretty HiFunPackBytes      = "pack-bytes"
  pretty HiFunUnpackBytes    = "unpack-bytes"
  pretty HiFunEncodeUtf8     = "encode-utf8"
  pretty HiFunDecodeUtf8     = "decode-utf8"
  pretty HiFunZip            = "zip"
  pretty HiFunUnzip          = "unzip"
  pretty HiFunSerialise      = "serialise"
  pretty HiFunDeserialise    = "deserialise"
  pretty HiFunRead           = "read"
  pretty HiFunWrite          = "write"
  pretty HiFunMkDir          = "mkdir"
  pretty HiFunChDir          = "cd"
  pretty HiFunParseTime      = "parse-time"
  pretty HiFunRand           = "rand"
  pretty HiFunEcho           = "echo"
  pretty HiFunCount          = "count"
  pretty HiFunKeys           = "keys"
  pretty HiFunValues         = "values"
  pretty HiFunInvert         = "invert"

--------------------------------------------------------------------------------
---- HiValue
--------------------------------------------------------------------------------

-- | HiValue represents values of Hi language.
data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString T.Text
  | HiValueList (S.Seq HiValue)
  | HiValueBytes BS.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (M.Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Serialise)

instance PP.Pretty HiValue where
  pretty (HiValueNumber val)    = let
    num = numerator val
    denom = denominator val
    (q, r) = quotRem num denom
    (sciVal, period) = fromRationalRepetendUnlimited val
    in case (q, r, period) of
      (_, 0, Nothing) -> PP.pretty q                                            -- integer
      (_, _, Nothing) -> PP.pretty $ formatScientific Fixed Nothing sciVal      -- finite decimal fractions
      (0, _, Just _)  -> PP.pretty r <> "/" <> PP.pretty denom                  -- fractions
      (_, _, Just _)  -> if r >= 0                                              -- mixed fractions
        then PP.pretty q <> " + " <> PP.pretty r <> "/" <> PP.pretty denom
        else PP.pretty q <> " - " <> PP.pretty (-r) <> "/" <> PP.pretty denom
  pretty (HiValueFunction fun)  = PP.pretty fun
  pretty (HiValueBool True)     = "true"
  pretty (HiValueBool False)    = "false"
  pretty HiValueNull            = "null"
  pretty (HiValueString text)   = PP.viaShow text
  pretty (HiValueList seq)      = if S.null seq
    then "[ ]"
    else PP.encloseSep "[ " " ]" ", " (PP.pretty <$> toList seq)
  pretty (HiValueBytes bytes)   = if BS.null bytes
    then "[# #]"
    else PP.encloseSep "[# " " #]" " " (PP.pretty . (printf "%02x" :: Word8 -> String) <$> BS.unpack bytes)
  pretty (HiValueAction action) = PP.pretty action
  pretty (HiValueTime time)     = "parse-time(\"" <> PP.viaShow time <> "\")"
  pretty (HiValueDict dict)     = if M.null dict
    then "{ }"
    else PP.encloseSep "{ " " }" ", " ((\(k,v) -> PP.pretty k <> ": " <> PP.pretty v) <$> M.toList dict)

-- | Defines set of types which can be converted to HiValue via 'toHiValue'.
class HiValueConvertable a where
  toHiValue :: a -> HiValue

instance HiValueConvertable HiValue where
  toHiValue = id

instance Integral a => HiValueConvertable (Ratio a) where
  toHiValue val = HiValueNumber $ toRational val

instance HiValueConvertable Int where
  toHiValue val = HiValueNumber $ toRational val

instance HiValueConvertable Word8 where
  toHiValue val = HiValueNumber $ toRational val

instance HiValueConvertable HiFun where
  toHiValue = HiValueFunction

instance HiValueConvertable Bool where
  toHiValue = HiValueBool

instance HiValueConvertable T.Text where
  toHiValue = HiValueString

instance HiValueConvertable Char where
  toHiValue c = HiValueString $ T.pack [c]

instance HiValueConvertable a => HiValueConvertable [a] where
  toHiValue vals = HiValueList $ S.fromList $ toHiValue <$> vals

instance HiValueConvertable a => HiValueConvertable (S.Seq a) where
  toHiValue vals = HiValueList $ toHiValue <$> vals

instance HiValueConvertable BS.ByteString where
  toHiValue bytes = HiValueBytes bytes

instance HiValueConvertable HiAction where
  toHiValue = HiValueAction

instance HiValueConvertable UTCTime where
  toHiValue = HiValueTime

instance (HiValueConvertable a, HiValueConvertable b) => HiValueConvertable (M.Map a b) where
  toHiValue dict = HiValueDict $ M.mapKeys toHiValue $ M.map toHiValue dict

--------------------------------------------------------------------------------
---- HiExpr
--------------------------------------------------------------------------------

-- | Represents parsed exppression of Hi language.
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

--------------------------------------------------------------------------------
---- HiError
--------------------------------------------------------------------------------

-- | Represents errors can be thrown during evaluating 'HiExpr'.
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

--------------------------------------------------------------------------------
---- HiAction
--------------------------------------------------------------------------------

-- | Represents actions, performed via system calls.
data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath BS.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving (Show, Eq, Ord, Generic, Serialise)

instance PP.Pretty HiAction where
  pretty (HiActionRead path)        = "read(" <> PP.viaShow path <> ")"
  pretty (HiActionWrite path bytes) = "write(" <> PP.viaShow path <> ", " <> PP.pretty (toHiValue bytes) <> ")"
  pretty (HiActionMkDir path)       = "mkdir(" <> PP.viaShow path <> ")"
  pretty (HiActionChDir path)       = "cd(" <> PP.viaShow path <> ")"
  pretty HiActionCwd                = "cwd"
  pretty HiActionNow                = "now"
  pretty (HiActionRand from to)     = "rand(" <> PP.viaShow from <> ", " <> PP.viaShow to <> ")"
  pretty (HiActionEcho msg)         = "echo(" <> PP.viaShow msg <> ")"

-- | Extension of 'Monad' with enviroment containing set of permissions.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
