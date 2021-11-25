{-# LANGUAGE LambdaCase #-}
module File where

import Ast (readExprList)
import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import Env (bindVars)
import Eval (eval, procedureBindings)
import Expr
  ( Env,
    Expr (Atom, List, String),
    IOThrowsError,
    Procedure,
    liftThrows,
  )
import System.IO (hPutStrLn, readFile, stderr, hPrint)

runFile :: [String] -> IO Env
runFile [] = procedureBindings
runFile args = do
  env <- procedureBindings
            >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runExceptT (show <$> eval env (List [Atom "load", String $ head args]))
    >>= (\case
            Left err -> hPrint stderr err
            Right val -> putStrLn val)
  return env
