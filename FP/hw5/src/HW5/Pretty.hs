module HW5.Pretty
  ( prettyValue
  ) where

import Prettyprinter (Doc, pretty, (<+>), encloseSep, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Data.Ratio
import Data.Scientific
import Data.Foldable
import Numeric (showHex)
import Data.Maybe
import Data.ByteString (ByteString, unpack)
import qualified Data.Map as M
import qualified Data.Text as T

import HW5.Base
import Data.Time (UTCTime)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction f)  = prettyFun f
prettyValue (HiValueNumber n)    = prettyRational n
prettyValue (HiValueBool b)      = prettyBool b
prettyValue HiValueNull          = pretty "null"
prettyValue (HiValueString s)    = prettyString (T.unpack s)
prettyValue (HiValueList l)      = prettyList (toList l)
prettyValue (HiValueBytes bytes) = prettyBytes bytes
prettyValue (HiValueDict dict)   = prettyDict dict
prettyValue (HiValueAction act)  = prettyAct act
prettyValue (HiValueTime time)   = prettyNow time

prettyNow :: UTCTime -> Doc AnsiStyle
prettyNow time = pretty "parse-time(\"" <> viaShow time <> pretty "\")"

prettyAct :: HiAction -> Doc AnsiStyle
prettyAct (HiActionWrite f s) = pretty "write(" <> prettyString f <> pretty ',' <> prettyBytes s <> pretty ")"
prettyAct (HiActionEcho s)    = pretty "echo(" <> prettyString (T.unpack s) <> pretty ")"
prettyAct (HiActionRead f)    = pretty "read(" <> prettyString f <> pretty ")"
prettyAct (HiActionMkDir f)   = pretty "mkdir(" <> prettyString f <> pretty ")"
prettyAct (HiActionChDir f)   = pretty "cd(" <> prettyString f <> pretty ")"
prettyAct HiActionCwd         = pretty "cwd"
prettyAct HiActionNow         = pretty "now"
prettyAct (HiActionRand l r)  = pretty "rand(" <> pretty l <> pretty ',' <> pretty r <> pretty ")"

prettyString :: String -> Doc AnsiStyle
prettyString text = pretty "\"" <> pretty text <> pretty "\""

prettyDict :: M.Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict = if M.null dict
  then pretty "{}"
  else encloseSep (pretty "{ ") (pretty " }") (pretty ", ") (prettyDictHelper <$> M.toList dict)

prettyDictHelper :: (HiValue, HiValue) -> Doc AnsiStyle
prettyDictHelper (a, b) = prettyValue a <> pretty ": " <> prettyValue b

prettyList :: [HiValue] -> Doc AnsiStyle
prettyList [] = pretty "[]"
prettyList l = encloseSep (pretty "[") (pretty "]") (pretty ", ") (prettyValue <$> l)

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bs = do
  let vals = fmap (\e -> if e < 16 then "0" ++ showHex e "" else showHex e "") (unpack bs)
  encloseSep (pretty "[# ") (pretty " #]" ) (pretty " ") (pretty <$> vals)

prettyBool :: Bool -> Doc AnsiStyle
prettyBool b = if b then pretty "true" else pretty "false"

prettyFun :: HiFun -> Doc AnsiStyle
prettyFun HiFunAdd            = pretty "add"
prettyFun HiFunSub            = pretty "sub"
prettyFun HiFunMul            = pretty "mul"
prettyFun HiFunDiv            = pretty "div"
prettyFun HiFunNot            = pretty "not"
prettyFun HiFunAnd            = pretty "and"
prettyFun HiFunOr             = pretty "or"
prettyFun HiFunLessThan       = pretty "less-than"
prettyFun HiFunGreaterThan    = pretty "greater-than"
prettyFun HiFunEquals         = pretty "equals"
prettyFun HiFunNotLessThan    = pretty "not-less-than"
prettyFun HiFunNotGreaterThan = pretty "not-greater-than"
prettyFun HiFunNotEquals      = pretty "not-equals"
prettyFun HiFunIf             = pretty "if"
prettyFun HiFunLength         = pretty "length"
prettyFun HiFunToUpper        = pretty "to-upper"
prettyFun HiFunToLower        = pretty "to-lower"
prettyFun HiFunReverse        = pretty "reverse"
prettyFun HiFunTrim           = pretty "trim"
prettyFun HiFunList           = pretty "list"
prettyFun HiFunRange          = pretty "range"
prettyFun HiFunFold           = pretty "fold"
prettyFun HiFunPackBytes      = pretty "pack-bytes"
prettyFun HiFunUnpackBytes    = pretty "unpack-bytes"
prettyFun HiFunEncodeUtf8     = pretty "encode-utf8"
prettyFun HiFunDecodeUtf8     = pretty "decode-utf8"
prettyFun HiFunZip            = pretty "zip"
prettyFun HiFunUnzip          = pretty "unzip"
prettyFun HiFunSerialise      = pretty "serialise"
prettyFun HiFunDeserialise    = pretty "deserialise"
prettyFun HiFunCount          = pretty "count"
prettyFun HiFunKeys           = pretty "keys"
prettyFun HiFunValues         = pretty "values"
prettyFun HiFunInvert         = pretty "invert"
prettyFun HiFunRead           = pretty "read"
prettyFun HiFunWrite          = pretty "write"
prettyFun HiFunMkDir          = pretty "mkdir"
prettyFun HiFunChDir          = pretty "cd"  
prettyFun HiFunParseTime      = pretty "parse-time" 
prettyFun HiFunRand           = pretty "rand" 
prettyFun HiFunEcho           = pretty "echo"

prettyRational :: Rational -> Doc AnsiStyle
prettyRational val = case denominator val of
  1 -> pretty (numerator val)
  val2 -> if isNothing (snd (fromRationalRepetendUnlimited val))
    then prettyFiniteDecimals (fst (fromRationalRepetendUnlimited val))
    else prettyMixedFrac (numerator val > 0) (abs (numerator val)) val2

prettyFiniteDecimals :: Scientific -> Doc AnsiStyle
prettyFiniteDecimals val = pretty (formatScientific Fixed Nothing val)

prettyMixedFrac :: Bool -> Integer -> Integer -> Doc AnsiStyle
prettyMixedFrac sign p q
  | p < q     = prettyEasyFrac sign p q
  | sign      = pretty (p `div` q) <+> pretty "+" <+> prettyFrac (p `mod` q) q
  | otherwise = pretty "-" <> pretty (p `div` q) <+> pretty "-" <+> prettyFrac (p `mod` q) q

prettyEasyFrac :: Bool -> Integer -> Integer -> Doc AnsiStyle
prettyEasyFrac sign p q
  | sign     = pretty p <> pretty "/" <> pretty q
  | otherwise = pretty "-" <> pretty p <> pretty "/" <> pretty q

prettyFrac :: Integer -> Integer -> Doc AnsiStyle
prettyFrac p q = pretty p <> pretty "/" <> pretty q
