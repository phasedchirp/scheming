{-# LANGUAGE ExistentialQuantification #-}

module Lib
    ( eval
    , LispVal(..)
    , showVal
    , readExpr
    , trapError
    , extractValue
    , ThrowsError
    , Env
    , runIOThrows
    , liftThrows
    , nullEnv
    ) where

import LispTypes -- Data types and closely associated functions
import StringOps -- functions for operating on strings
import ParseExpr -- input parsing
import Control.Monad (liftM)
import Control.Monad.Trans.Except (throwE,catchE,Except(..),runExcept,ExceptT(..),runExceptT,except)
import Text.Megaparsec (ParseError,parse)
import qualified Data.Map as M (Map(..), lookup, insert, fromList)
import Data.IORef


-- Lookup table of primitive functions:
primitives :: M.Map String ([LispVal] -> ThrowsError LispVal)
primitives = M.fromList [("+", numericBinop (+)),
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
              ("string-ci=?", strCiBoolBinop (==)),
              ("string-ci<?", strCiBoolBinop (<)),
              ("string-ci>?", strCiBoolBinop (>)),
              ("string-ci<=?", strCiBoolBinop (<=)),
              ("string-ci>=?", strCiBoolBinop (>=)),
              ("make-string", makeString),
              ("string", string),
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("string-set!", stringSet),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string-copy", stringCopy),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
              ]


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwE $ Parser err
     Right val -> return val



apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwE $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (M.lookup func primitives)

eval :: Env -> LispVal -> IOThrowsError LispVal
-- Evaluation for basic types:
eval env val@(String _) = return val
eval env val@(Character _) = return val
eval env val@(Bool _) = return val
eval env val@(Number _) = return val
eval env val@(Complex _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio _) = return val
-- Not-exactly-primative expressions
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of Bool False -> eval env alt
                 otherwise  -> eval env conseq
          -- Alternative approach if you want only bools to be conditional-y, drop the otherwise clause and add these two
                -- Bool True -> eval env conseq
               -- otherwise -> throwE $ TypeMismatch "non-bool" result

eval env (List [Atom "cond"]) = throwE $ Default "insufficient cases for cond"
eval env (List ((Atom "cond"):(List [opt]):opts)) = do
  test <- eval env opt
  case test of Bool True -> return $ Bool True
               Bool False -> eval env (List ((Atom "cond"):opts))
               otherwise -> throwE (Default "invalid conditional form")
eval env (List ((Atom "cond"):(List [opt,expr]):opts)) = do
  test <- eval env opt
  expr' <- eval env expr
  case test of Bool True -> return expr'
               Bool False -> eval env (List ((Atom "cond"):opts))
               otherwise -> throwE (Default "invalid conditional form")

eval env (List [Atom "case"]) = throwE $ Default "insufficient cases for cond"
eval env (List ((Atom "case"):key:(List [datums,expr]):rest)) = do
  key' <- eval env key
  datums' <- eval env datums
  expr' <- eval env expr
  case datums' of List vals -> if key' `elem` vals then return expr' else eval env (List ((Atom "case"):key:rest))
                  otherwise -> throwE $ Default "something went wrong"

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env val@(List _) = return val
eval env badForm = throwE $ BadSpecialForm "Unrecognized special form" badForm


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
strCiBoolBinop = boolBinop unpackCiStr
boolBoolBinop = boolBinop unpackBool

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
