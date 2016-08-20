module Lib where

import Control.Monad (liftM)
import Numeric (readHex,readOct,readInt)
import Data.Char (digitToInt)
import Text.Megaparsec hiding (spaces,space,lexeme)
import Text.Megaparsec.String

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
             deriving (Show)

--  Parsers:

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--  replicating space
spaces :: Parser ()
spaces = skipSome spaceChar

escapes = [('b','\b'),('n','\n'),('f','\f'),('r','\r'),('t','\t'),('\\','\\'),('"','"')]

escapes' = map (\(x, y) -> char x >> return y) escapes

escapeChar :: Parser Char
escapeChar = char '\\' >> choice escapes'


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ (escapeChar <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  rest <- many (letterChar <|> digitChar <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom


parseDecimal :: Parser LispVal
parseDecimal = some digitChar >>= \x -> return $ (Number . read) x

readOct' = fst . head . readOct
parseOctal :: Parser LispVal
parseOctal = some octDigitChar >>= \x -> return $ (Number . readOct') x

readHex' = fst . head . readHex
parseHex :: Parser LispVal
parseHex = some hexDigitChar >>= \x -> return $ (Number . readHex') x

-- Probably a better way to do this?
readBin = readInt 2 (`elem` "01") digitToInt
readBin' = fst . head . readBin
parseBin :: Parser LispVal
parseBin = some digitChar >>= \x -> return $ (Number . readBin') x

parseBase :: Parser LispVal
parseBase = do
  radix <- char '#' >> letterChar
  case radix of 'd' -> parseDecimal
                'o' -> parseOctal
                'x' -> parseHex
                'b' -> parseBin

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseBase


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseExpr' :: Parser LispVal
parseExpr' = spaces >> parseExpr

-- Read input:
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match" ++ show err
  Right _ -> "Found value"
