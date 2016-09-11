module Main where

import System.Environment (getArgs)
import Control.Monad (liftM, forever)
import Lib
import ParseExpr
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM showVal $ (liftThrows $ readExpr expr) >>= eval env

-- evalString :: String -> IO String
-- evalString expr = return $ extractValue $ trapError (liftM showVal $ readExpr expr >>= eval)


repl :: Env -> IO ()
repl lispEnv = forever $ do
  putStr "Lisp>>> "
  lispExpr <- getLine
  -- putStrLn lispExpr
  -- let lispExpr = readMaybe val :: Maybe String
  case lispExpr of "quit" -> exitSuccess
                   otherwise -> evalAndPrint lispEnv lispExpr >> repl lispEnv



-- parseArg :: IO (ThrowsError LispVal)
-- parseArg = do
--   args <- getArgs
--   let lispExpr = readExpr (head args) >>= eval
--   return lispExpr

main :: IO ()
main = do
  args <- getArgs
  lispEnv <- nullEnv
  case length args of 0 -> do
                            hSetBuffering stdout NoBuffering
                            repl lispEnv
                      1 -> evalAndPrint lispEnv $ head args
                      otherwise -> putStrLn "Program takes only 0 or 1 arguments"

    --  args <- getArgs
    --  evaled <- return $ liftM showVal $ readExpr (args !! 0) >>= eval
    --  putStrLn $ extractValue $ trapError evaled
