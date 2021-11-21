module IoProcedures where

import Ast (readExpr, readExprList)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Expr
  ( Expr (Bool, List, Port, String),
    IOProcedure,
    SchemeError (NumArgs, TypeMismatch),
    liftThrows, IOThrowsError
  )
import System.IO (IOMode, hClose, hGetLine, hPrint, openFile, stdin, stdout)

makePort :: IOMode -> IOProcedure
makePort mode [String file] = Port <$> liftIO (openFile file mode)
makePort _ [v] = throwError $ TypeMismatch "string" v
makePort _ v = throwError $ NumArgs 1 v

closePort :: IOProcedure
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort [_] = return $ Bool False
closePort v = throwError $ NumArgs 1 v

readProc :: IOProcedure
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [v] = throwError $ TypeMismatch "port" v
readProc v = throwError $ NumArgs 1 v

writeProc :: IOProcedure
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc [_, v] = throwError $ TypeMismatch "port" v
writeProc v = throwError $ NumArgs 1 v

readContents :: IOProcedure
readContents [String filename] = String <$> liftIO (readFile filename)
readContents [v] = throwError $ TypeMismatch "string" v
readContents v = throwError $ NumArgs 1 v

load :: String -> IOThrowsError [Expr]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: IOProcedure
readAll [String filename] = List <$> load filename
readAll [v] = throwError $ TypeMismatch "string" v
readAll v = throwError $ NumArgs 1 v
