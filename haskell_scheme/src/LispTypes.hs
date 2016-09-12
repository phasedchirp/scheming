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
             deriving (Eq)

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

-- extractValue :: ThrowsError a -> a
-- extractValue x = case runExcept x of (Right val) -> val



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
