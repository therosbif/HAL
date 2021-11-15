module Repl where

import System.IO
import Ast (readExpr)
import Eval (eval)
import Expr (showErr, extractValue)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

getInput :: String -> IO String
getInput s = flushStr s >> getLine

evalStr :: String -> IO String
evalStr s =
  let evaled = show <$> (readExpr s >>= eval)
  in return $ extractValue $ showErr evaled

printEvalStr :: String -> IO ()
printEvalStr s = evalStr s >>= putStrLn

replLoop_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
replLoop_ pred prompt action =
  prompt >>= (\res -> if pred res
                        then return ()
                        else action res >> replLoop_ pred prompt action)

runRepl :: IO ()
runRepl = replLoop_ (== "exit") (getInput "HAL *> ") printEvalStr