module Repl where

import System.IO
import Ast (readExpr)
import Eval (eval)
import Error (extractValue, showErr, liftThrows, runIOThrows)
import Env (Env, emptyEnv)
import GHC.IO (catch)
import System.IO.Error (isEOFError)
import GHC.Exception (SomeException(SomeException))

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

getInput :: String -> IO String
getInput s =  flushStr s >> catch getLine
                              (\e -> if isEOFError e
                                      then return "quit"
                                      else hPrint stderr e >> return "")

evalStr :: Env -> String -> IO String
evalStr env s =
  runIOThrows $ fmap show $ liftThrows (readExpr s) >>= eval env

printEvalStr :: Env -> String -> IO ()
printEvalStr env s = evalStr env s >>= putStrLn

replLoop_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
replLoop_ pred prompt action =
  prompt >>= (\res -> if pred res
                        then return ()
                        else action res >> replLoop_ pred prompt action)

runOnce :: String -> IO ()
runOnce s = emptyEnv >>= flip printEvalStr s

runRepl :: IO ()
runRepl = emptyEnv >>= replLoop_ (== "quit") (getInput "HAL *> ") . printEvalStr
