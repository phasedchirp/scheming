module Lib where

import ParseExpr
import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Text.Megaparsec (ParseError,parse)


-- data LispNumber = Float Double
--                 | Rational Rational
--                 | Integer Integer
--                 deriving (Eq,Show)
--
-- data LComplex =




data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String



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
