{-# LANGUAGE ExistentialQuantification #-}

module Lib
    ( eval
    , LispVal(..)
    , showVal
    , readExpr
    , trapError
    , extractValue
    , ThrowsError
    ) where

import ParseExpr
import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Text.Megaparsec (ParseError,parse)


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
              ("string->symbol", unaryOp atomify),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("cond", cond)]



data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String


-- Needed for implementation of `equal?` below
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type ThrowsError = Except LispError

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
-- Conditionals
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of Bool False -> eval alt
                       Bool True  -> eval conseq
                       otherwise  -> throwE $ TypeMismatch "non-bool" result
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwE $ BadSpecialForm "Unrecognized special form" badForm

-- List handling stuff:
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]          = return x
car [DottedList (x : _) _]  = return x
car [badArg]                = throwE $ TypeMismatch "pair" badArg
car badArgList              = throwE $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] y]      = return $ y
cdr [DottedList (_ : xs) y] = return $ DottedList xs y
cdr [badArg]                = throwE $ TypeMismatch "pair" badArg
cdr badArgList              = throwE $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]           = return $ List [x]
cons [x, List xs]           = return $ List (x:xs)
cons [x, DottedList xs y]   = return $ DottedList (x:xs) y
cons [x, y]                 = return $ DottedList [x] y
cons badArgList             = throwE $ NumArgs 2 badArgList

-- Regular equivalence checking
eqv :: [LispVal] -> ThrowsError LispVal
eqv args = case args of [arg1, arg2] -> return $ Bool $ (arg1 == arg2)
                        badArgList   -> throwE $ NumArgs 2 badArgList


-- Equivalence checking with type coercion
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchE` (const $ return False)

zipLists :: [LispVal] -> [LispVal] -> [[LispVal]]
zipLists = zipWith (\x y -> [x,y])

listEqual :: [[LispVal]] -> Bool
listEqual = lispAnd . (map equal)

lispAnd :: [ThrowsError LispVal] -> Bool
lispAnd [] = True
lispAnd (x:xs) = case xE of (Bool val) -> val && (lispAnd xs)
                            otherwise -> False
  where xE = either (const (Bool False)) id (runExcept x)

equal :: [LispVal] -> ThrowsError LispVal
equal args = case args of [List xs, List ys] -> if (length xs == length ys)
                                                  then return $ Bool $ listEqual (zipLists xs ys)
                                                  else return $ Bool False
                          [DottedList xs x, DottedList ys y] -> do
                            (Bool listEq) <- equal [List xs, List ys]
                            (Bool tailEq) <- equal [x,y]
                            return $ Bool $ tailEq && listEq
                          [arg1,arg2] -> do
                            primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                               [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                            (Bool eqvEquals) <- eqv [arg1, arg2]
                            return $ Bool $ (primitiveEquals || eqvEquals)
equal badArgList = throwE $ NumArgs 2 badArgList

cond :: [LispVal] -> ThrowsError LispVal
cond [] = throwE (Default "no default case provided")
cond (test:ts) = do
    (Bool test') <- evalTest
    expr' <- expr
    if test'
      then return expr'
      else cond ts
  where (evalTest,expr) = case test of List [t]                -> (eval t, eval t)
                                       List [t,e@(List exprs)] -> (eval t, eval e)
                                      --  otherwise               -> (throwE (Default "unrecognized conditional form")


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwE $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)




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



boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwE $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwE $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwE $ TypeMismatch "boolean" notBool



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


trapError action = catchE action (return . showError)

extractValue :: ThrowsError a -> a
extractValue x = case runExcept x of (Right val) -> val
