-- Basic types and some associated functions
{-# LANGUAGE ExistentialQuantification #-}
module LispTypes where

import Control.Monad.Trans.Except (Except(..),except,runExcept,throwE,catchE,ExceptT(..),runExceptT)
import Data.Map (Map(..))
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Data.Complex (Complex(..))
import Data.Ratio
import qualified Data.Vector as V (Vector)
import Text.Megaparsec (ParseError)
import System.IO (Handle)


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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
            --  deriving (Eq)
-- Having functions as part of LispVal rules out deriving automatically
-- Better way to handle this?
instance Eq LispVal where
  Atom x == Atom y = x == y
  List a == List b = a == b
  DottedList xs y == DottedList as b = (xs == as) && (y == b)
  Vector vs == Vector vs' = vs == vs'
  Number a == Number b = a == b
  Float a == Float b = a == b
  Ratio a == Ratio b = a == b
  Complex a == Complex b = a == b
  String a == String b = a == b
  Bool a == Bool b = a == b
  _ == _ = False


-- Printing
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = "#\\" ++ [contents]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func pars varargs body closure) = "(lambda (" ++ unwords (map show pars) ++ arg ++ ")...)"
  where arg = case varargs of Nothing -> []
                              Just val -> " . " ++ val
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Error Handling:
data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ showVal form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ showVal found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = message


-- Needed for implementation of `equal?`
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- Error handling using ExceptT
type ThrowsError = Except LispError

extractValue :: ThrowsError a -> a
extractValue x = case runExcept x of (Right val) -> val

trapError :: Monad m => ExceptT LispError m String -> ExceptT e' m String
trapError action = catchE action (return . showError)


-- Defining variables and passing around state:

-- Type for enviroments to be passed around:
type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows x = case runExcept x of Left err -> throwE err
                                   Right val -> return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = (runExceptT $ trapError action) >>= return . extractValue . except

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwE $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwE $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
