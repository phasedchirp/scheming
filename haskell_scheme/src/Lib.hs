module Lib where

import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Data.Complex (Complex(..))
import Data.Ratio
import qualified Data.Vector as V
import Numeric (readHex,readOct,readInt)
import Data.Char (digitToInt)
import Text.Megaparsec hiding (spaces,space,lexeme)
import Text.Megaparsec.String
import Text.Megaparsec.Combinator (sepBy,endBy)

-- data LispNumber = Float Double
--                 | Rational Rational
--                 | Integer Integer
--                 deriving (Eq,Show)
--
-- data LComplex =

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (V.Vector LispVal)
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double) -- should be more general!
             | Character Char
             | String String
             | Bool Bool
            --  deriving (Show)


data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

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

-- parseBin = char '#' >> char 'b' >> parseBin'
-- parseHex = char '#' >> char 'x' >> parseHex'

-- has issues when used in conjuction
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

-- Read input:
-- readExpr :: String -> LispVal
-- readExpr input = case parse parseExpr "lisp" input of
--   Left err -> String $ "No match" ++ show err
--   Right val -> val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwE $ Parser err
     Right val -> return val


-- Do stuff with input:
-- Printing
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- instance Except LispError where
    --  noMsg = Default "An error has occurred"
    --  strMsg = Default


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Complex _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwE $ BadSpecialForm "Unrecognized special form" badForm


-- apply :: String -> [LispVal] -> LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwE $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", unaryOp isBool),
              ("symbol?", unaryOp isSymbol),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("symbol->string", unaryOp stringify),
              ("string->symbol", unaryOp atomify)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
-- unaryOp f [] = throwE $ NumArgs 1 []
unaryOp f _ = throwE $ NumArgs 1 []

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwE $ NumArgs 2 []
numericBinop op singleVal@[_] = throwE $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwE $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwE $ TypeMismatch "number" notNum


floatBinop :: (Double -> Double -> Double) -> [LispVal] -> LispVal
floatBinop op params = Float $ foldl1 op $ map unpackFloat params

unpackFloat :: LispVal -> Double
unpackFloat (Float n) = n
unpackFloat _ = 0.0


-- truthy :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
-- truthy f = Bool . (all f)

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False


stringify (Atom s)   = String s
stringify _ = String ""
atomify (String s) = Atom s
atomify _ = Atom ""



-- Error Handling:
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ showVal form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ showVal found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

type ThrowsError = Except LispError

trapError action = catchE action (return . showError)

extractValue :: ThrowsError a -> a
extractValue x = case runExcept x of (Right val) -> val
