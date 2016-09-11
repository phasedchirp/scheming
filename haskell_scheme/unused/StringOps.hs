module StringOps where

-- Functions for operating on strings

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwE $ TypeMismatch "string" notString

unpackCiStr :: LispVal -> ThrowsError String
unpackCiStr (String s) = return $ map toLower s
unpackCiStr s = unpackStr s

makeString :: [LispVal] -> ThrowsError LispVal
makeString [] = throwE $ NumArgs 1 []
makeString [x] = do
  val <- eval x
  case val of Number n -> return $ String $ replicate (fromIntegral n) ' '
              otherwise -> throwE $ Default "make-string! takes an integer"
makeString [x,y] = do
  val <- eval x
  c <- eval y
  case (val,c) of (Number n, Character x) -> return $ String $ replicate (fromIntegral n) x
                  otherwise -> throwE $ Default "make-string! takes an integer and character"


appendChars :: LispVal -> LispVal -> LispVal
appendChars (Character c1) (Character c2) = String $ c1:c2:[]
appendChars (Character c1) (String s) = String $ c1:s
-- appendChars _ _ = throwE $ Default "tried to append non-characters"

string :: [LispVal] -> ThrowsError LispVal
string [] = return $ String []
string xs = do
  c <- sequence $ map eval xs
  return $ foldr appendChars (String []) c

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

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List xs] =
  let isChar (Character _) = True
      isChar _ = False
  in do
    c <- sequence $ map eval xs
    if all isChar c
      then return $ foldr appendChars (String []) c
      else throwE $ Default "invalid arguments to list->string"
