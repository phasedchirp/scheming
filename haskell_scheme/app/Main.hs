module Main where

import System.Environment
import Control.Monad (liftM)
import Lib
import ParseExpr

parseArg :: IO (ThrowsError LispVal)
parseArg = do
  args <- getArgs
  let lispExpr = readExpr (head args) >>= eval
  return lispExpr

main :: IO ()
main = do
  evaled <- parseArg
  evaled' <- return $ liftM showVal evaled
  putStrLn $ extractValue $ trapError evaled'
    --  args <- getArgs
    --  evaled <- return $ liftM showVal $ readExpr (args !! 0) >>= eval
    --  putStrLn $ extractValue $ trapError evaled
