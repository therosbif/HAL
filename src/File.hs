module File where

import Ast (readExprList)
import Control.Monad.Except (MonadIO (liftIO))
import Env (bindVars)
import Eval (eval, procedureBindings)
import Expr
  ( Env,
    Expr (Atom, List, String),
    IOThrowsError,
    Procedure,
    liftThrows,
    runIOThrows,
  )
import System.IO (hPutStrLn, readFile, stderr)

handleFile :: String -> String
handleFile f = f --do
--xs <- lines <$> readFile f
--interpret (unwords xs)

runFile :: [String] -> IO Env
runFile [] = procedureBindings
runFile args = do
  env <- procedureBindings
            >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows
    (show <$> eval env (List [Atom "load", String $ head args]))
    >>= hPutStrLn stderr
  return env
