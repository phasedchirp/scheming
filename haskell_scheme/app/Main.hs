module Main where

import System.Environment
import Control.Monad (liftM, forever)
import Lib
import ParseExpr
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import System.IO

-- flushStr :: String -> IO ()
-- flushStr str = putStr str >> hFlush stdout

-- readPrompt :: String -> IO String
-- readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM showVal $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

repl :: IO ()
repl = forever $ do
  putStr "Lisp>>> "
  lispExpr <- getLine
  -- putStrLn lispExpr
  -- let lispExpr = readMaybe val :: Maybe String
  case lispExpr of "quit" -> exitSuccess
                   otherwise -> evalAndPrint lispExpr >> repl



parseArg :: IO (ThrowsError LispVal)
parseArg = do
  args <- getArgs
  let lispExpr = readExpr (head args) >>= eval
  return lispExpr

main :: IO ()
main = do
  args <- getArgs
  case length args of 0 -> do
                            hSetBuffering stdout NoBuffering
                            repl
                      1 -> evalAndPrint $ head args
                      otherwise -> putStrLn "Program takes only 0 or 1 arguments"

    --  args <- getArgs
    --  evaled <- return $ liftM showVal $ readExpr (args !! 0) >>= eval
    --  putStrLn $ extractValue $ trapError evaled
