module StringOps
    ( unpackStr
    , unpackCiStr
    , stringLength
    , string
    , stringRef
    , stringSet
    , substring
    , stringAppend
    , stringCopy
    , stringToList
    , listToString
    , makeString
    ) where

import LispTypes (LispVal(..),ThrowsError(..),LispError(..))
import Control.Monad.Trans.Except (throwE,catchE,Except(..),runExcept,ExceptT(..),runExceptT,except)
import Data.Char (toLower)

-- Functions for operating on strings

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwE $ TypeMismatch "string" notString

unpackCiStr :: LispVal -> ThrowsError String
unpackCiStr (String s) = return $ map toLower s
unpackCiStr s = unpackStr s

appendChars :: LispVal -> LispVal -> LispVal
appendChars (Character c1) (Character c2) = String $ c1:c2:[]
appendChars (Character c1) (String s) = String $ c1:s

string :: [LispVal] -> ThrowsError LispVal
string [] = return $ String []
string xs = return $ foldr appendChars (String []) xs

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [] = throwE $ NumArgs 0 []
stringLength [String xs] = return $ Number $ fromIntegral (length xs)
stringLength _ = throwE $ Default "invalid arguments to string-length"

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [Number n, String xs] = return $ Character $ xs !! (fromIntegral n)
stringRef _ = throwE $ Default "invalid arguments to string-ref"

insertChar :: Integer -> Char -> String -> String
insertChar _ _ [] = []
insertChar k c (x:xs) | k < 0 = xs
                      | k == 0 = c:xs
                      | otherwise = x:(insertChar (k-1) c xs)

stringSet :: [LispVal] -> ThrowsError LispVal
stringSet [String xs, Number n, Character c] = return $ String $ insertChar n c xs
stringSet _ = throwE $ Default "invalid arguments to string-set!"

getSubstring :: Integer -> Integer -> String -> String
getSubstring _ _ [] = []
getSubstring f l cs = take (l' - f') $ drop f' cs
  where [f',l'] = map fromIntegral [f,l]

substring :: [LispVal] -> ThrowsError LispVal
substring [String cs, Number f, Number l] | (l+1) <= (fromIntegral $ length cs) = return $ String $ getSubstring f l cs
                                          | otherwise = throwE $ Default "indexing error in substring"
substring _ = throwE $ Default "invalid arguments to substring"

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [String xs, String ys] = return $ String $ xs ++ ys
stringAppend _ = throwE $ Default "invalid arguments to string-append"

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String xs] = return $ String xs
stringCopy _ = throwE $ Default "invalid arguments to string-copy"

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String xs] = return $ List $ map Character xs

makeString :: [LispVal] -> ThrowsError LispVal
makeString [] = throwE $ NumArgs 1 []
makeString [x] = case x of Number n -> return $ String $ replicate (fromIntegral n) ' '
                           otherwise -> throwE $ Default "make-string! takes an integer"
makeString [x,y] = case (x,y) of (Number n, Character c) -> return $ String $ replicate (fromIntegral n) c
                                 otherwise -> throwE $ Default "make-string! takes an integer and character"

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List xs] =
  let isChar (Character _) = True
      isChar _ = False
  in
    if all isChar xs
      then return $ foldr appendChars (String []) xs
      else throwE $ Default "invalid arguments to list->string"
