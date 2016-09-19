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
    , primitiveBindings
    , runFile
    ) where

import LispTypes -- Data types and closely associated functions
import Data.Complex (Complex(..))
import Data.Ratio
import StringOps -- functions for operating on strings
import ParseExpr -- input parsing
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Control.Monad.Trans.Except (throwE,catchE,Except(..),runExcept,ExceptT(..),runExceptT,except)
import Text.Megaparsec (ParseError,parse)
import Text.Megaparsec.String (Parser(..))
import Text.Megaparsec.Combinator (endBy)
import qualified Data.Map as M (Map(..), lookup, insert, fromList)
import Data.IORef
import System.IO


-- Lookup table of primitive functions:
primitives :: [(String,[LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop "+"),
              ("-", numericBinop "-"),
              ("*", numericBinop "*"),
              ("/", numericBinop "/"),
              ("mod", numericBinop "mod"),
              ("quotient", numericBinop "quot"),
              ("remainder", numericBinop "rem"),
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

-- Primitives for IO handling
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ (map (makeFunc IOFunc) ioPrimitives) ++ (map (makeFunc PrimitiveFunc) primitives))
  where makeFunc constructor (var,func) = (var, constructor func)



--
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwE $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args  | nPars /= nArgs && varargs == Nothing = throwE  $ NumArgs (fromIntegral nPars) args
                                               | otherwise = (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
                                                  where nPars = length params
                                                        nArgs = length args
                                                        remainingArgs = drop nPars args
                                                        evalBody env = liftM last $ mapM (eval env) body
                                                        bindVarArgs arg env  = case arg of Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                                                                                           Nothing -> return env


--
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args


-- Evaluation for basic types:
eval :: Env -> LispVal -> IOThrowsError LispVal
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

eval env (List (Atom "define" : List (Atom var : params):body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var:params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" :varargs@(Atom _):body)) = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (func : args)) = do
  func' <- eval env func
  argVals <- mapM (eval env) args
  apply func' argVals
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


runIntOp op pars = case lookup op intOps of Just f -> integerBinop f pars
                                            otherwise -> throwE $ Default "unrecognized primitive function for ints"

runFloOp op pars = case lookup op floatOps of Just f -> floatBinop f pars
                                              otherwise -> throwE $ Default "unrecognized primitive function for floats"

runRatOp op pars = case lookup op ratioOps of Just f -> rationalBinop f pars
                                              otherwise -> throwE $ Default "unrecognized primitive function for ratios"

runComOp op pars = case lookup op complexOps of Just f -> complexBinop f pars
                                                otherwise -> throwE $ Default "unrecognized primitive function for complex numbers"

numericBinop _ []    = throwE $ NumArgs 2 []
numericBinop _ [x]   = throwE $ NumArgs 2 [x]
numericBinop op pars = case pars of ((Number _):ps)  -> runIntOp op pars
                                    ((Float _):ps)   -> runFloOp op pars
                                    ((Ratio _):ps)   -> runRatOp op pars
                                    ((Complex _):ps) -> runComOp op pars

-- Handling Integers
intOps = [("+",(+)),("-",(-)),("*",(*)),("/",div),("mod",mod),("quot",quot),("rem",rem)]

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwE $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwE $ TypeMismatch "number" notNum

-- handling floats
floatOps = [("+",(+)),("-",(-)),("*",(*)),("/",(/))]

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n) = return n
unpackFloat (String n) = let parsed = reads n in
                         if null parsed
                             then throwE $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackFloat (List [n]) = unpackFloat n
unpackFloat notNum     = throwE $ TypeMismatch "number" notNum

floatBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
floatBinop op params        = mapM unpackFloat params >>= return . Float . foldl1 op

-- handling rational
ratioOps = [("+",(+)),("-",(-)),("*",(*)),("/",(/))]

unpackRational :: LispVal -> ThrowsError Rational
unpackRational (Ratio n) = return n
unpackRational (String n) = let parsed = reads n in
                            if null parsed
                              then throwE $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackRational (List [n]) = unpackRational n
unpackRational notNum     = throwE $ TypeMismatch "number" notNum

rationalBinop :: (Rational -> Rational -> Rational) -> [LispVal] -> ThrowsError LispVal
rationalBinop op params        = mapM unpackRational params >>= return . Ratio . foldl1 op
--
-- handling Complex
complexOps = [("+",(+)),("-",(-)),("*",(*)),("/",(/))]
unpackComplex :: LispVal -> ThrowsError (Complex Double)
unpackComplex (Complex n) = return n
unpackComplex (String n) = let parsed = reads n in
                            if null parsed
                              then throwE $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackComplex (List [n]) = unpackComplex n
unpackComplex notNum     = throwE $ TypeMismatch "number" notNum
--
complexBinop :: (Complex Double -> Complex Double -> Complex Double) -> [LispVal] -> ThrowsError LispVal
complexBinop op params        = mapM unpackComplex params >>= return . Complex . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwE $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


intBoolBinop = boolBinop unpackNum
floatBoolBinOp = boolBinop unpackFloat
ratioBoolBinOp = boolBinop unpackRational
-- complexBoolBinop = boolBinop unpackComplex

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


-- Handling IO
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port (showVal obj) >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

runFile :: [String] -> IO ()
runFile args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM showVal $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr
