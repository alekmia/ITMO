{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , HiAction (..)
  , HiMonad (..)
  ) where
import Data.Text
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Codec.Serialise
import GHC.Generics
import Data.Map
import Data.Time.Clock

data HiFun = HiFunAdd
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
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiValue = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueDict (Map HiValue HiValue)
  | HiValueAction HiAction
  | HiValueTime UTCTime
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiExpr = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)]
  | HiExprRun HiExpr
  deriving (Show, Eq, Ord)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic, Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

data HiError = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving Show
