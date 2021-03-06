module ParseExpr
    (parseExpr,
     parseExpr',
     LispVal(..),
     spaces
    ) where


import LispTypes (LispVal(..))
import Data.Complex (Complex(..))
import Data.Ratio
import qualified Data.Vector as V (Vector,fromList,(//),(!))
import Numeric (readHex,readOct,readInt)
import Data.Char (digitToInt)
import Text.Megaparsec (oneOf,skipSome,some,letterChar,spaceChar,char,digitChar,choice
                        ,(<|>),try,noneOf,many,octDigitChar,hexDigitChar,printChar) --hiding (spaces,space,lexeme)
import Text.Megaparsec.String (Parser(..))
import Text.Megaparsec.Combinator (sepBy,endBy)
import Control.Monad (liftM)

--  Parsers:

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--  replicating space
spaces :: Parser ()
spaces = skipSome spaceChar

parseChar :: Parser LispVal
parseChar = do
  char '#' >> char '\\'
  first <- printChar
  rest <- many (noneOf " ()[]\\\t\n\r\"")
  case rest of [] -> return $ Character first
               otherwise -> case first:rest of "space" -> return $ Character ' '
                                               "newline" -> return $ Character '\n'

-- Handling escape characters. Mostly based on something in ch. 16 of Real World Haskell
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


parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


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

parseDouble :: Parser LispVal
parseDouble = do
  x <- some digitChar
  char '.'
  y <- some digitChar
  return $ Float (read $ x ++ "." ++ y)

parseRational :: Parser LispVal
parseRational = do
  x <- some digitChar
  char '/'
  y <- some digitChar
  return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do
  r <- some digitChar
  char '+'
  i <- some digitChar
  char 'j'
  return $ Complex ((read r) :+ (read i))

parseNumeric :: Parser LispVal
parseNumeric = try parseComplex
           <|> try parseDouble
           <|> try parseRational
           <|> try parseDecimal
           <|> try parseBase

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]



parseList :: Parser LispVal
parseList  = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head' <- endBy parseExpr spaces
  tail' <- char '.' >> spaces >> parseExpr
  return $ DottedList head' tail'

tryLists :: Parser LispVal
tryLists = do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseVector :: Parser LispVal
parseVector = do
  char '#' >> char '('
  xs <- sepBy parseExpr spaces
  char ')'
  return $ (Vector . V.fromList) xs

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseChar
        <|> parseBool
        <|> parseNumeric
        <|> parseQuoted
        <|> parseQuasiquote
        <|> parseUnquote
        <|> tryLists

parseExpr' :: Parser LispVal
parseExpr' = spaces >> parseExpr
