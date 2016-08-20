module Lib where

import Text.Megaparsec hiding (spaces,space,lexeme)
import Text.Megaparsec.String

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--  replicating space
spaces :: Parser ()
spaces = skipSome spaceChar

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match" ++ show err
  Right val -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
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
