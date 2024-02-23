{-# LANGUAGE LambdaCase #-}

module HW5.Evaluator
  ( eval
  ) where

import Control.Monad.Except
import Control.Applicative
import Control.Monad.Trans.Except
import Data.Text as T
import Data.Semigroup (Semigroup(stimes))
import Data.Ratio (denominator, numerator)
import qualified Data.Sequence as S
import Data.Foldable as F
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as L
import Data.Word (Word8)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Codec.Serialise
import Codec.Compression.Zlib
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Text.Read (readMaybe)

import HW5.Base

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
-- eval :: HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (f expr)

f :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
f expr = case expr of
   HiExprValue v -> return v
   HiExprApply v args -> applyHelper (HiExprApply v args)
   HiExprDict dict -> dictHelper dict
   HiExprRun run -> do
    evaled <- f run
    runHelper evaled

runHelper :: HiMonad m => HiValue -> ExceptT HiError m HiValue
runHelper (HiValueAction act) = ExceptT (fmap Right (runAction act))
runHelper _ = throwE HiErrorInvalidFunction

applyHelper :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
applyHelper (HiExprApply func list) = do
  hiValueFun <- f func
  case hiValueFun of
    (HiValueFunction HiFunAnd) -> lazyAnd list
    (HiValueFunction HiFunOr) -> lazyOr list
    (HiValueFunction HiFunIf) -> lazyIf list
    _ -> do
      unwrapped <- traverse f list
      applyDifferent hiValueFun unwrapped
applyHelper _ = throwE HiErrorInvalidFunction

dictHelper :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
dictHelper dict = do
  unwrappedDict <- traverse (\(key, value) -> liftA2 (,) (f key) (f value)) dict
  return (HiValueDict (M.fromList unwrappedDict))

applyDifferent :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyDifferent (HiValueFunction fun) args = applyFun fun args
applyDifferent (HiValueString str) args = applyStrFun str args
applyDifferent (HiValueList list) args = applyListFun list args
applyDifferent (HiValueBytes bytes) args = applyBytesFun bytes args
applyDifferent (HiValueDict dict) args = applyDictFun dict args
applyDifferent _ _ = throwE HiErrorInvalidFunction

applyDictFun :: HiMonad m => M.Map HiValue HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyDictFun dict [a] = if M.member a dict then return (dict M.! a) else return HiValueNull
applyDictFun _ _ = throwE HiErrorInvalidArgument

applyBytesFun :: HiMonad m => B.ByteString -> [HiValue] -> ExceptT HiError m HiValue
applyBytesFun str [HiValueNumber a]
  | denominator a == 1 = if checkValid B.length str a then
    return (getByIndexBytes str (numerator a))
    else return HiValueNull
  | otherwise = throwE HiErrorInvalidArgument
applyBytesFun str [HiValueNumber a, HiValueNumber b]
  | denominator a == 1 && denominator b == 1 = if not (a >= 0 && b >= 0 && a > b) then
    return (HiValueBytes (getSlice B.drop B.take str (numerator a) (numerator b)))
    else return HiValueNull
  | otherwise = throwE HiErrorInvalidArgument
applyBytesFun _ _ = throwE HiErrorInvalidArgument

applyListFun :: HiMonad m => S.Seq HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyListFun list [HiValueNumber a]
  | denominator a == 1 = if checkValid S.length list a then
    return (getByIndexList list (numerator a))
    else return HiValueNull
  | otherwise = throwE HiErrorInvalidArgument
applyListFun list [HiValueNumber a, HiValueNumber b]
  | denominator a == 1 && denominator b == 1 = if not (a >= 0 && b >= 0 && a > b) then
    return (HiValueList (getSlice S.drop S.take list (numerator a) (numerator b)))
    else return HiValueNull
  | otherwise = throwE HiErrorInvalidArgument
applyListFun _ _ = throwE HiErrorInvalidArgument

applyStrFun :: HiMonad m => Text -> [HiValue] -> ExceptT HiError m HiValue
applyStrFun str [HiValueNumber a]
  | denominator a == 1 = if checkValid T.length str a then
    return (getByIndex str (numerator a))
    else return HiValueNull
  | otherwise = throwE HiErrorInvalidArgument
applyStrFun str [HiValueNumber a, HiValueNumber b]
  | denominator a == 1 && denominator b == 1 = if not (a >= 0 && b >= 0 && a > b) then
    return (HiValueString (getSlice T.drop T.take str (tranformIndex (numerator a) (fromIntegral (T.length str))) (tranformIndex (numerator b) (fromIntegral (T.length str)))))
    else return HiValueNull
  | otherwise = throwE HiErrorInvalidArgument
applyStrFun str [HiValueNull, HiValueNumber b] = applyStrFun str [HiValueNumber 0, HiValueNumber b]
applyStrFun str [HiValueNumber a, HiValueNull] = applyStrFun str [HiValueNumber a, HiValueNumber (toRational (T.length str))]
applyStrFun _ _ = throwE HiErrorInvalidArgument

checkValid :: (t1 -> Int) -> t1 -> Rational -> Bool
checkValid fun list a = numerator a >= 0 && numerator a < fromIntegral (fun list)

tranformIndex :: Integer -> Integer -> Integer
tranformIndex i str =
  if i < 0 then i + str else i

getByIndex :: Text -> Integer -> HiValue
getByIndex str a = HiValueString (singleton (index str (fromIntegral a)))

getByIndexList :: S.Seq HiValue -> Integer -> HiValue
getByIndexList list a = S.index list (fromIntegral a)

getByIndexBytes :: B.ByteString -> Integer -> HiValue
getByIndexBytes bytes a = HiValueNumber (toRational (toInteger (B.index bytes (fromIntegral a))))

getSlice :: (Int -> t -> t) -> (Int -> t -> t) -> t -> Integer -> Integer -> t
getSlice dropFun takeFun str a b = takeFun (fromIntegral (b - a)) (dropFun (fromIntegral a) str)

lazyAnd :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
lazyAnd [a, b] = do
  evalA <- f a
  case evalA of
    HiValueBool False -> do f a
    HiValueNull -> do f a
    _ -> do f b
lazyAnd _ = throwE HiErrorArityMismatch

lazyOr :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
lazyOr [a, b] = do
  evalA <- f a
  case evalA of
    HiValueBool False -> do f b
    HiValueNull -> do f b
    _ -> do f a
lazyOr _ = throwE HiErrorArityMismatch

lazyIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
lazyIf [a, b, c] = do
  evalA <- f a
  case evalA of
    HiValueBool True -> do f b
    HiValueBool False -> do f c
    _ -> throwE HiErrorInvalidArgument
lazyIf _ = throwE HiErrorArityMismatch

applyFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
applyFun HiFunAdd [HiValueTime t, HiValueNumber n]                 = applyAddTime t n
applyFun HiFunSub [HiValueTime t1, HiValueTime t2]                 = applyDeltaTime t1 t2
--
applyFun HiFunAdd [HiValueNumber a, HiValueNumber b]               = applyFunHelper (+) (HiValueNumber a) (HiValueNumber b)
applyFun HiFunSub [a, b]                                           = applyFunHelper (-) a b
applyFun HiFunMul [HiValueNumber a, HiValueNumber b]               = applyFunHelper (*) (HiValueNumber a) (HiValueNumber b)
applyFun HiFunDiv [_, HiValueNumber 0]                             = throwE HiErrorDivideByZero
applyFun HiFunDiv [HiValueNumber a, HiValueNumber b]               = applyFunHelper (/) (HiValueNumber a) (HiValueNumber b)
--
applyFun HiFunAdd [HiValueString a, HiValueString b]               = applyMathString append (HiValueString a) (HiValueString b)
applyFun HiFunMul [HiValueString a, HiValueNumber b]               = repeatValue (HiValueString a) (HiValueNumber b)
applyFun HiFunDiv [HiValueString a, HiValueString b]               = applyMathString concatWithSlash (HiValueString a) (HiValueString b)
--
applyFun HiFunAdd [HiValueBytes a, HiValueBytes b]                 = applyMathBytes B.append a b
applyFun HiFunMul [HiValueBytes a, HiValueNumber b]                = repeatValue (HiValueBytes a) (HiValueNumber b)
--
applyFun HiFunAdd [HiValueList a, HiValueList b]                   = applyMathList mappend a b
applyFun HiFunMul [HiValueList a, HiValueNumber b]                 = repeatValue (HiValueList a) (HiValueNumber b)
-- 
applyFun HiFunNot [a]                  = applyUnaryBoolFunHelper not a
--
applyFun HiFunLessThan [a, b]          = compFunHelper (<) a b
applyFun HiFunGreaterThan [a, b]       = compFunHelper (>) a b
applyFun HiFunEquals [a, b]            = compFunHelper (==) a b
applyFun HiFunNotLessThan [a, b]       = compFunHelper (>=) a b
applyFun HiFunNotGreaterThan [a, b]    = compFunHelper (<=) a b
applyFun HiFunNotEquals [a, b]         = compFunHelper (/=) a b
--
applyFun HiFunLength [HiValueString a]  = applyLength T.length a
applyFun HiFunLength [HiValueList a]    = applyLength S.length a
applyFun HiFunToUpper [HiValueString a] = applyStringTransform T.toUpper a
applyFun HiFunToLower [HiValueString a] = applyStringTransform T.toLower a
applyFun HiFunReverse [HiValueString a] = applyStringTransform T.reverse a
applyFun HiFunReverse [HiValueList a]   = applyListTransform S.reverse a
applyFun HiFunTrim [HiValueString a]    = applyStringTransform T.strip a
--
applyFun HiFunList list                                    = formList list
applyFun HiFunRange [a, b]                                 = formRange a b
applyFun HiFunFold [HiValueFunction fun, HiValueList list] = applyFold fun list
--
applyFun HiFunPackBytes [list]            = applyPackBytes list
applyFun HiFunUnpackBytes [bytestring]    = applyUnpackBytes bytestring
applyFun HiFunEncodeUtf8 [string]         = applyEncodeBytes string
applyFun HiFunDecodeUtf8 [byteString]     = applyDecodeBytes byteString
applyFun HiFunSerialise [a]               = applySerialiseBytes a
applyFun HiFunDeserialise [a]             = applyDeserialiseBytes a
applyFun HiFunZip [a]                     = applyCompressionBytes (compressWith defaultCompressParams { compressLevel = bestCompression }) a
applyFun HiFunUnzip [a]                   = applyCompressionBytes (decompressWith defaultDecompressParams) a
--
applyFun HiFunKeys [a]                    = applyGetListDict M.keys a
applyFun HiFunValues [a]                  = applyGetListDict M.elems a
applyFun HiFunInvert [a]                  = applyInvertDict a
applyFun HiFunCount [HiValueString s]     = applyCountString s
applyFun HiFunCount [HiValueBytes b]      = applyCountBytes b
applyFun HiFunCount [HiValueList l]       = applyCountList l
--
applyFun HiFunRead [HiValueString a]                   = return (HiValueAction (HiActionRead (T.unpack a)))
applyFun HiFunWrite [HiValueString a, HiValueString b] = return (HiValueAction (HiActionWrite (T.unpack a) (encodeUtf8 b)))
applyFun HiFunMkDir [HiValueString a]                  = return (HiValueAction (HiActionMkDir (T.unpack a)))
applyFun HiFunChDir [HiValueString a]                  = return (HiValueAction (HiActionChDir (T.unpack a)))
applyFun HiFunEcho [HiValueString a]                   = return (HiValueAction (HiActionEcho a))
applyFun HiFunParseTime [HiValueString s]              = applyParseTime s
applyFun HiFunRand [HiValueNumber a, HiValueNumber b]  = applyRand a b
--
applyFun _ _ = throwE HiErrorArityMismatch

applyRand :: HiMonad m => Rational -> Rational -> ExceptT HiError m HiValue
applyRand a b = if denominator a == 1 && denominator b == 1 then
  return (HiValueAction (HiActionRand (fromIntegral (numerator a)) (fromIntegral (numerator b))))
  else
    return HiValueNull

applyAddTime :: HiMonad m => UTCTime -> Rational -> ExceptT HiError m HiValue
applyAddTime t n = return (HiValueTime (addUTCTime (fromRational n) t))

applyDeltaTime :: HiMonad m => UTCTime -> UTCTime -> ExceptT HiError m HiValue
applyDeltaTime t1 t2 = return (HiValueNumber (toRational (diffUTCTime t1 t2)))

applyParseTime :: HiMonad m => Text -> ExceptT HiError m HiValue
applyParseTime str = return (maybe HiValueNull HiValueTime (readMaybe (T.unpack str)))

applyCountList :: HiMonad m => S.Seq HiValue -> ExceptT HiError m HiValue
applyCountList list = do
  let elDict = M.fromListWith (+) [(c, 1) | c <- F.toList list]
  return (HiValueDict (M.map HiValueNumber elDict))

applyCountBytes :: HiMonad m => B.ByteString -> ExceptT HiError m HiValue
applyCountBytes bytes = do
  let byteDict = M.fromListWith (+) [(c, 1) | c <- B.unpack bytes]
  let numDict = M.mapKeys (HiValueNumber . toRational . toInteger) (M.map HiValueNumber byteDict)
  return (HiValueDict numDict)

applyCountString :: HiMonad m => Text -> ExceptT HiError m HiValue
applyCountString str = do
  let charDict = M.fromListWith (+) [(c, 1) | c <- T.unpack str]
  let strDict = M.mapKeys (HiValueString . T.singleton) (M.map HiValueNumber charDict)
  return (HiValueDict strDict)

applyInvertDict :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applyInvertDict (HiValueDict dict) = do
  let inverted = M.fromListWith (++) [(v, [k]) | (k, v) <- M.toList dict]
  return (HiValueDict (M.map (HiValueList . S.fromList) inverted))
applyInvertDict _ = throwE HiErrorInvalidArgument

applyGetListDict :: HiMonad m => (M.Map HiValue HiValue -> [HiValue]) -> HiValue -> ExceptT HiError m HiValue
applyGetListDict fun (HiValueDict dict) = return (HiValueList (S.fromList (fun dict)))
applyGetListDict _ _ = throwE HiErrorInvalidArgument

applyCompressionBytes :: HiMonad m => (L.ByteString -> L.ByteString) -> HiValue -> ExceptT HiError m HiValue
applyCompressionBytes fun (HiValueBytes obj) = return (HiValueBytes (toStrict (fun (fromStrict obj))))
applyCompressionBytes _ _ = throwE HiErrorInvalidArgument

applyDeserialiseBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applyDeserialiseBytes (HiValueBytes obj) = return (deserialise (fromStrict obj))
applyDeserialiseBytes _ = throwE HiErrorInvalidArgument

applySerialiseBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applySerialiseBytes obj = return (HiValueBytes (toStrict (serialise obj)))

applyEncodeBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applyEncodeBytes (HiValueString string) = return (HiValueBytes (encodeUtf8 string))
applyEncodeBytes _ = throwE HiErrorInvalidArgument

applyDecodeBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applyDecodeBytes (HiValueBytes bytes) = return (either (const HiValueNull) HiValueString (decodeUtf8' bytes))
applyDecodeBytes _ = throwE HiErrorInvalidArgument

applyUnpackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applyUnpackBytes (HiValueBytes bytes) = do
  let word8 = B.unpack bytes
  let ints = fmap (HiValueNumber . toRational . toInteger) word8
  return (HiValueList (S.fromList ints))
applyUnpackBytes _ = throwE HiErrorInvalidArgument

applyPackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
applyPackBytes (HiValueList seqhi) = do
  let list = F.toList seqhi
  ratios <- traverse (\case
                  (HiValueNumber n) -> return n
                  _ -> throwE HiErrorInvalidArgument) list
  ints <- traverse byteArgChecker ratios
  return (HiValueBytes (B.pack ints))
applyPackBytes _ = throwE HiErrorInvalidArgument

byteArgChecker :: HiMonad m => Rational -> ExceptT HiError m Word8
byteArgChecker e = if denominator e == 1 && numerator e >= 0 && numerator e <= 255
  then return (fromIntegral (numerator e))
  else throwE HiErrorInvalidArgument

applyFold :: HiMonad m => HiFun -> S.Seq HiValue -> ExceptT HiError m HiValue
applyFold fun list = do
  case F.toList list of
    [] -> return HiValueNull
    _ -> F.foldl1 (foldHelper fun) (return <$> list)

foldHelper :: HiMonad m => HiFun -> ExceptT HiError m HiValue -> ExceptT HiError m HiValue -> ExceptT HiError m HiValue
foldHelper fun fe se = do
        unwrappedFe <- fe
        unwrappedSe <- se
        applyFun fun [unwrappedFe, unwrappedSe]

applyMathList :: HiMonad m => (S.Seq HiValue -> S.Seq HiValue -> S.Seq HiValue) -> S.Seq HiValue -> S.Seq HiValue -> ExceptT HiError m HiValue
applyMathList fun a b = return (HiValueList (fun a b))

applyMathBytes :: HiMonad m => (B.ByteString -> B.ByteString -> B.ByteString) -> B.ByteString -> B.ByteString -> ExceptT HiError m HiValue
applyMathBytes fun a b = return (HiValueBytes (fun a b))

formRange :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
formRange (HiValueNumber start) (HiValueNumber end) = do
  let rng =  fmap HiValueNumber [start..end]
  return (HiValueList (S.fromList rng))
formRange _ _ = throwE HiErrorInvalidArgument

formList :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
formList list = return (HiValueList (S.fromList list))

concatWithSlash :: Text -> Text -> Text
concatWithSlash a b = append a (append (pack "/") b)

repeatValue :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
repeatValue (HiValueString a) (HiValueNumber n)
  | denominator n == 1 = return (HiValueString (stimes (numerator n) a))
  | otherwise          = throwE HiErrorInvalidArgument
repeatValue (HiValueList a) (HiValueNumber n)
  | denominator n == 1 = return (HiValueList (stimes (numerator n) a))
  | otherwise          = throwE HiErrorInvalidArgument
repeatValue (HiValueBytes b) (HiValueNumber n)
  | denominator n == 1 = return (HiValueBytes (stimes (numerator n) b))
  | otherwise          = throwE HiErrorInvalidArgument
repeatValue _ _ = throwE HiErrorInvalidArgument

applyMathString :: HiMonad m => (Text -> Text -> Text) -> HiValue -> HiValue -> ExceptT HiError m HiValue
applyMathString fun (HiValueString a) (HiValueString b) = return (HiValueString (fun a b))
applyMathString _ _ _ = throwE HiErrorInvalidArgument

applyStringTransform :: HiMonad m => (Text -> Text) -> Text -> ExceptT HiError m HiValue
applyStringTransform fun a = return (HiValueString (fun a))

applyListTransform :: HiMonad m => (S.Seq HiValue -> S.Seq HiValue) -> S.Seq HiValue -> ExceptT HiError m HiValue
applyListTransform fun a = return (HiValueList (fun a))

applyLength :: HiMonad m => (t -> Int) -> t -> ExceptT HiError m HiValue
applyLength fun a = return (HiValueNumber (toRational (fun a)))

compFunHelper :: HiMonad m => (HiValue -> HiValue -> Bool) -> HiValue -> HiValue -> ExceptT HiError m HiValue
compFunHelper fun a b = return (HiValueBool (fun a b))

applyUnaryBoolFunHelper :: HiMonad m => (Bool -> Bool) -> HiValue -> ExceptT HiError m HiValue
applyUnaryBoolFunHelper fun (HiValueBool a) = return (HiValueBool (fun a))
applyUnaryBoolFunHelper _ _ = throwE HiErrorInvalidArgument

applyFunHelper :: HiMonad m => (Rational -> Rational -> Rational) -> HiValue -> HiValue -> ExceptT HiError m HiValue
applyFunHelper fun (HiValueNumber a) (HiValueNumber b) = return (HiValueNumber (fun a b))
applyFunHelper _ _ _ = throwE HiErrorInvalidArgument
