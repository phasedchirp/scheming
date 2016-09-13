module Main where

import System.Environment (getArgs)
import Control.Monad (liftM, forever)
import Lib
import ParseExpr
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering),hPutStrLn,stderr)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM showVal $ (liftThrows $ readExpr expr) >>= eval env


repl :: Env -> IO ()
repl lispEnv = forever $ do
  putStr "Lisp>>> "
  lispExpr <- getLine
  case lispExpr of "quit" -> exitSuccess
                   otherwise -> evalAndPrint lispEnv lispExpr >> repl lispEnv


-- runFile :: [String] -> IO ()
-- runFile args = do
--     env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
--     (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
--         >>= hPutStrLn stderr


main :: IO ()
main = do
  args <- getArgs
  lispEnv <- primitiveBindings
  case length args of 0 -> do
                            hSetBuffering stdout NoBuffering
                            repl lispEnv
                      otherwise -> runFile $ args
