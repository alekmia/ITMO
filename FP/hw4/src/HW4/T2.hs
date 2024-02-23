{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char

import HW4.Types
import HW4.T1 (ExceptState(..), runES)

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Applies given parser on given string and returns result
runP :: Parser a -> String -> Except ParseError a
runP (P p) str = case runES p (0, str) of
  Error e -> Error e
  Success (a :# _) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
-- | Parser that consumes one char. Was given in HW files as an example
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | Parser that always throws an error
parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P p) (P q) = P $ ES $ \(pos, s) ->
    case runES p (pos, s) of
      Error _ -> runES q (pos, s)
      Success a -> Success a


-- No metohds
instance MonadPlus Parser

{-|
  Takes in a string and parses it. Returns the result which is 
  Either 'Error' and tells the position of failure
  Either 'Success' and the parsed expression.

  Production rules used as the grammar: 

  parserE  -> parserT parserE' 
  
  parserE' -> + parserT parserE' | - parserT parserE' | epsilon
  
  parserT  -> parserF parserT'
  
  parserT' -> * parserF parserT' | / parserF parserT' | epsilon
  
  parserF  -> ( parserE ) | int
-}
parseExpr :: String -> Except ParseError Expr
parseExpr = runP parser

parser :: Parser Expr
parser = do
  parseSpaces
  val <- parserE
  parseSpaces
  pEof
  return val

parserE :: Parser Expr
parserE = do
  val <- parserT
  parserE' val <|> return val

parserE' :: Expr -> Parser Expr
parserE' e = do
  parseSpaces
  op <- parseOpCheck '+' <|> parseOpCheck '-'
  parseSpaces
  val <- parserT
  te' <- case op of
    '+' -> return (Op (Add e val))
    '-' -> return (Op (Sub e val))
    _ -> parseError
  parserE' te' <|> return te'

parserT :: Parser Expr
parserT = do
  val <- parserF
  parserT' val <|> return val

parserT' :: Expr -> Parser Expr
parserT' e = do
  parseSpaces
  op <- parseOpCheck '*' <|> parseOpCheck '/'
  parseSpaces
  val <- parserF
  ft' <- case op of
    '*' -> return (Op (Mul e val))
    '/' -> return (Op (Div e val))
    _ -> parseError
  parseSpaces
  parserT' ft' <|> return ft'

parserF :: Parser Expr
parserF = parseInBrackets <|> parseDouble <|> parseInt

parseInBrackets :: Parser Expr
parseInBrackets = do
  parseSyntaxCheck '('
  parseSpaces
  val <- parserE
  parseSpaces
  parseSyntaxCheck ')'
  return val

parseSyntaxCheck :: Char -> Parser ()
parseSyntaxCheck c = void (mfilter (== c) pChar)

parseOpCheck :: Char -> Parser Char
parseOpCheck c = mfilter (== c) pChar

parseSpaces :: Parser ()
parseSpaces = void (many (mfilter Data.Char.isSpace pChar))

parseInt :: Parser Expr
parseInt = do
  val <- parseIntAsString
  return (Val (fromIntegral (stringToInt val)))

parseDouble :: Parser Expr
parseDouble = do
  val1 <- parseIntAsString
  parseSyntaxCheck '.'
  val2 <- parseIntAsString
  let a = fromIntegral (stringToInt val1)
  let b = fromIntegral (stringToInt val2)
  let l = fromIntegral (length val2)
  return (Val (a + (b / (10 ** l))))

parseIntAsString :: Parser String
parseIntAsString = some (mfilter Data.Char.isDigit pChar)

stringToInt :: String -> Int
stringToInt = foldl (\res s -> res * 10 + Data.Char.digitToInt s) 0

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    []     -> Success (() :# (pos, s))
    _ -> Error (ErrorAtPos pos)
