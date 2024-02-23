module HW5.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Control.Monad (void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (pack)
import Data.Char
import qualified Data.List as LI
import qualified Data.ByteString as B
import Data.Scientific (Scientific)

import HW5.Base

type Parser = Parsec Void String

pBool :: Parser HiExpr
pBool = choice
  [ HiExprValue (HiValueBool True) <$ string "true"
  , HiExprValue (HiValueBool False) <$ string "false" ]

pNull :: Parser HiExpr
pNull = do
  void (string "null")
  return (HiExprValue HiValueNull)

pOper :: Parser HiFun
pOper = choice
  [ HiFunAdd            <$ string "add"
  , HiFunSub            <$ string "sub"
  , HiFunMul            <$ string "mul"
  , HiFunDiv            <$ string "div"
  , HiFunAnd            <$ string "and"
  , HiFunOr             <$ string "or"
  , HiFunLessThan       <$ string "less-than"
  , HiFunGreaterThan    <$ string "greater-than"
  , HiFunEquals         <$ string "equals"
  , HiFunNotLessThan    <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals      <$ string "not-equals"
  , HiFunNot            <$ string "not"
  , HiFunIf             <$ string "if"
  , HiFunLength         <$ string "length"
  , HiFunToUpper        <$ string "to-upper"
  , HiFunToLower        <$ string "to-lower"
  , HiFunReverse        <$ string "reverse"
  , HiFunTrim           <$ string "trim"
  , HiFunList           <$ string "list"
  , HiFunRange          <$ string "range"
  , HiFunFold           <$ string "fold"
  , HiFunPackBytes      <$ string "pack-bytes"
  , HiFunUnpackBytes    <$ string "unpack-bytes"
  , HiFunEncodeUtf8     <$ string "encode-utf8"
  , HiFunDecodeUtf8     <$ string "decode-utf8"
  , HiFunZip            <$ string "zip"
  , HiFunUnzip          <$ string "unzip"
  , HiFunSerialise      <$ string "serialise"
  , HiFunDeserialise    <$ string "deserialise"
  , HiFunCount          <$ string "count"
  , HiFunKeys           <$ string "keys"
  , HiFunValues         <$ string "values"
  , HiFunInvert         <$ string "invert"
  , HiFunRead           <$ string "read"
  , HiFunWrite          <$ string "write"
  , HiFunMkDir          <$ string "mkdir"
  , HiFunChDir          <$ string "cd"
  , HiFunParseTime      <$ string "parse-time"
  , HiFunRand           <$ string "rand"
  , HiFunEcho           <$ string "echo"
  ]

wrapFuncToApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
wrapFuncToApply fun a b = HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]

converter :: [[Operator Parser HiExpr]]
converter = [ [ binary (wrapFuncToApply HiFunMul) "*"
              , divide (wrapFuncToApply HiFunDiv) ]
            , [ binary (wrapFuncToApply HiFunAdd) "+"
              , binary (wrapFuncToApply HiFunSub) "-" ]
            , [ binaryUnique (wrapFuncToApply HiFunNotLessThan) ">="
              , binaryUnique (wrapFuncToApply HiFunNotGreaterThan) "<="
              , binaryUnique (wrapFuncToApply HiFunLessThan) "<"
              , binaryUnique (wrapFuncToApply HiFunGreaterThan) ">"
              , binaryUnique (wrapFuncToApply HiFunEquals) "=="
              , binaryUnique (wrapFuncToApply HiFunNotEquals) "/=" ]
            , [ binaryBool (wrapFuncToApply HiFunAnd) "&&"]
            , [ binaryBool (wrapFuncToApply HiFunOr) "||"] ]


binary :: (HiExpr -> HiExpr -> HiExpr) -> String -> Operator Parser HiExpr
binary fun opName = InfixL (fun <$ symbol opName)

binaryUnique :: (HiExpr -> HiExpr -> HiExpr) -> String -> Operator Parser HiExpr
binaryUnique fun opName = InfixN (fun <$ symbol opName)

binaryBool :: (HiExpr -> HiExpr -> HiExpr) -> String -> Operator Parser HiExpr
binaryBool fun opName = InfixR (fun <$ symbol opName)

divide :: (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
divide fun = InfixL (fun <$ (lexeme . try) (string "/" <* notFollowedBy (satisfy (== '='))))

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pConvert <* eof) ""

pConvert :: Parser HiExpr
pConvert = makeExprParser pExpr converter

pFunc :: Parser HiExpr
pFunc = do
  oper <- pOper
  return (HiExprValue (HiValueFunction oper))

noBracketsExpr :: Parser HiExpr
noBracketsExpr = do
  funToApply <- pSpaces *> pV <* pSpaces
  pTopper funToApply <|> return funToApply

withBracketsExpr :: Parser HiExpr
withBracketsExpr = do
  val <- brackets pConvert
  pTopper val <|> return val

pList :: Parser HiExpr
pList = do
  vals <- sqBrackets pArgs
  let list = HiExprApply (HiExprValue (HiValueFunction HiFunList)) vals
  pTopper list <|> return list

pBytes :: Parser HiExpr
pBytes = do
  vals <- byteBrackets (sepEndBy pByteArg space1)
  let intVals = fmap fromIntegral vals
  let byteString = HiExprValue (HiValueBytes (B.pack intVals))
  pTopper byteString <|> return byteString

pDict :: Parser HiExpr
pDict = do
  entries <- dictBrackets (sepBy pDictEntry (pSpaces *> char ',' <* pSpaces))
  pTopper (HiExprDict entries) <|> return (HiExprDict entries)

pExpr :: Parser HiExpr
pExpr = noBracketsExpr <|> withBracketsExpr <|> pBytes <|> pDict <|> pList

brackets :: Parser HiExpr -> Parser HiExpr
brackets p = symbol "(" *> p <* symbol ")"

argsBrackets :: Parser [HiExpr] -> Parser [HiExpr]
argsBrackets p = symbol "(" *> p <* symbol ")"

sqBrackets :: Parser [HiExpr] -> Parser [HiExpr]
sqBrackets p = symbol "[" *> p <* symbol "]"

byteBrackets :: Parser [Integer] -> Parser [Integer]
byteBrackets p = symbol "[#" *> pSpaces *> p <* symbol "#]"

dictBrackets :: Parser [(HiExpr, HiExpr)] -> Parser [(HiExpr, HiExpr)]
dictBrackets p = symbol "{" *> p <* symbol "}"

pV :: Parser HiExpr
pV = pNumber <|> pBool <|> pNull <|> pString <|> pFunc <|> pCwd <|> pNow

pE' :: HiExpr -> Parser HiExpr
pE' fun = do
  args <- argsBrackets pArgs
  void pSpaces
  pTopper (HiExprApply fun args) <|> return (HiExprApply fun args)

pD' :: HiExpr -> Parser HiExpr
pD' fun = do
  fld <- HiExprApply fun . (\e -> [HiExprValue (HiValueString (pack e))]) <$> (char '.' *> identifier)
  pTopper fld <|> return fld

pA' :: HiExpr -> Parser HiExpr
pA' fun = do
  void (char '!')
  pSpaces
  pTopper (HiExprRun fun) <|> return (HiExprRun fun)

pTopper :: HiExpr -> Parser HiExpr
pTopper fun = pE' fun <|> pD' fun <|> pA' fun

identifier :: Parser String
identifier = LI.intercalate "-" <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')

pArgs :: Parser [HiExpr]
pArgs = do
  args <- sepBy pConvert (pSpaces *> char ',' <* pSpaces)
  return args

pByteArg :: Parser Integer
pByteArg = do
  b1 <- hexDigitChar
  b2 <- hexDigitChar
  return (read ("0x" ++ [b1, b2]))

pDictEntry :: Parser (HiExpr, HiExpr)
pDictEntry = do
  key <- pConvert
  pSpaces
  void (char ':')
  pSpaces
  value <- pConvert
  return (key, value)

pNow :: Parser HiExpr
pNow = do
  void (string "now")
  return (HiExprValue (HiValueAction HiActionNow))

pCwd :: Parser HiExpr
pCwd = do
  void (string "cwd")
  return (HiExprValue (HiValueAction HiActionCwd))

pNumber :: Parser HiExpr
pNumber = do
  numScientific <- pSignedNumber
  return (HiExprValue (HiValueNumber (toRational numScientific)))

pSignedNumber :: Parser Scientific
pSignedNumber = L.signed sc L.scientific

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

pString :: Parser HiExpr
pString = do
  str <- stringLiteral
  return (HiExprValue (HiValueString (pack str)))

pSpaces :: Parser ()
pSpaces = void $ many (char ' ')

sc :: Parser ()
sc = L.space (void $ some (char ' ')) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser String -> Parser String
lexeme = L.lexeme sc
