-- Basic types and some associated functions
{-# LANGUAGE ExistentialQuantification #-}
module LispTypes where

import Control.Monad.Trans.Except (Except(..))
import Data.Map (Map(..))


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



data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

--
-- Error Handling:
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


trapError action = catchE action (return . showError)

extractValue :: ThrowsError a -> a
extractValue x = case runExcept x of (Right val) -> val

-- Evaluation:

apply :: Map -> String -> [LispVal] -> ThrowsError LispVal
apply primitives func args = maybe (throwE $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (M.lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Complex _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
-- Conditionals
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of Bool False -> eval alt
                       Bool True  -> eval conseq
                       otherwise  -> throwE $ TypeMismatch "non-bool" result
eval (List (Atom func : args)) = mapM eval args >>= apply  primitives func
eval val@(List _) = return val
eval badForm = throwE $ BadSpecialForm "Unrecognized special form" badForm
