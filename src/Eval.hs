{-# LANGUAGE LambdaCase #-}

module Eval where

import Ast (readExpr)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO), MonadTrans (lift))
import Data.Maybe (isNothing)
import Env (bindVars, defineVar, emptyEnv, getVar, setVar)
import Expr
  ( Env,
    Expr
      ( Atom,
        Bool,
        DottedList,
        Func,
        IOFunc,
        List,
        Number,
        PrimitiveFunc,
        SpecicalFunc,
        String
      ),
    IOProcedure,
    IOThrowsError,
    Procedure,
    SchemeError
      ( Default,
        NotFunction,
        NumArgs,
        SpecialFormErr,
        TypeMismatch
      ),
    SpecialForm,
    ThrowsError,
    liftThrows,
  )
import IoProcedures
  ( closePort,
    makePort,
    readAll,
    readContents,
    readProc,
    writeProc, load
  )
import Procedures
  ( arithmeticProcedures,
    booleanProcedures,
    listProcedures,
  )
import SpecialForms (lambda, quote)
import System.IO (IOMode (ReadMode, WriteMode))
import Utils (makeNormalFunc, makeVarargs, unpackList)

procedures :: [([Char], Procedure)]
procedures =
  arithmeticProcedures
    ++ booleanProcedures
    ++ listProcedures

specialForms :: [(String, SpecialForm)]
specialForms =
  [ ("cond", cond),
    ("if", schemeIf),
    ("define", define),
    ("set!", set),
    ("lambda", lambda),
    ("let", schemeLet),
    ("quote", quote),
    ("load", schemeLoad)
  ]

ioProcedures :: [(String, IOProcedure)]
ioProcedures =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

procedureBindings :: IO Env
procedureBindings =
  emptyEnv
    >>= flip bindVars (map (makeFunc PrimitiveFunc) procedures)
    >>= flip bindVars (map (makeFunc SpecicalFunc) specialForms)
    >>= flip bindVars (map (makeFunc IOFunc) ioProcedures)
  where
    makeFunc c (key, func) = (key, c func)

--------------------------------------------------------------------------------

cond :: SpecialForm
cond _ [] = throwError $ Default "No catch-all condition."
cond env (List [pred, conseq] : xs) =
  eval env pred
    >>= ( \case
            Bool False -> cond env xs
            Bool True -> eval env conseq
            v -> throwError $ TypeMismatch "bool" v
        )
cond _ (List e : _) = throwError $ NumArgs 2 e
cond _ (e : _) = throwError $ TypeMismatch "list" e

schemeIf :: SpecialForm
schemeIf env [pred, conseq, alt] =
  eval env pred
    >>= ( \case
            Bool True -> eval env conseq
            Bool False -> eval env alt
            v -> throwError $ TypeMismatch "boolean" v
        )
schemeIf _ v = throwError $ NumArgs 3 v

define :: SpecialForm
define env [Atom key, val] = eval env val >>= defineVar env key
define env (List (Atom key : params) : body) =
  makeNormalFunc env params body >>= defineVar env key
define env (DottedList (Atom key : params) varargs : body) =
  makeVarargs varargs env params body >>= defineVar env key
define _ [key, _] = throwError $ TypeMismatch "list" key
define _ args = throwError $ NumArgs 2 args

set :: SpecialForm
set env [Atom key, val] = eval env val >>= setVar env key
set _ [key, _] = throwError $ TypeMismatch "atom" key
set _ args = throwError $ NumArgs 2 args

schemeLoad :: SpecialForm
schemeLoad env [String file] = load file >>= fmap last . mapM (eval env)
schemeLoad _ [v] = throwError $ TypeMismatch "string" v
schemeLoad _ v = throwError $ NumArgs 1 v

schemeLet :: SpecialForm
schemeLet env [List params, expr] = do
  pairs <- liftThrows $ makePairs params
  e <- lift $ bindVars env pairs
  eval e expr
schemeLet _ [a, _] = throwError $ TypeMismatch "list" a
schemeLet _ a = throwError $ NumArgs 2 a

makePairs :: [Expr] -> ThrowsError [(String, Expr)]
makePairs (List [Atom key, val] : xs) =
  makePairs xs >>= \s -> return $ (key, val) : s
makePairs (List [key, _] : _) = throwError $ TypeMismatch "atom" key
makePairs (List x : _) = throwError $ NumArgs 2 x
makePairs (a : _) = throwError $ TypeMismatch "list" a
makePairs [] = return []

--------------------------------------------------------------------------------

bindVarArgs :: Maybe String -> [Expr] -> Env -> IOThrowsError Env
bindVarArgs arg remainingArgs env =
  case arg of
    Nothing -> return env
    Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]

applyProc :: IOProcedure
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc [] = throwError $ NumArgs 1 []

apply :: Expr -> [Expr] -> IOThrowsError Expr
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params vaarg body closure) args
  | num params /= num args && isNothing vaarg =
    throwError $ NumArgs (num params) args
  | otherwise = liftIO (bindVars closure $ zip params args)
                  >>= bindVarArgs vaarg (drop (length params) args) >>= evalBody
                    where
                      num = toInteger . length
                      evalBody env = last <$> mapM (eval env) body
apply f _ = throwError $ TypeMismatch "function" f

eval :: Env -> Expr -> IOThrowsError Expr
eval _ v@(String _) = return v
eval _ v@(Number _) = return v
eval _ v@(Bool _) = return v
eval env (Atom id) = getVar env id
eval env (List [v]) = eval env v
eval env (List (func : args)) =
  eval env func >>= (\case
                        SpecicalFunc f -> f env args
                        f -> mapM (eval env) args >>= apply f)
eval _ v = throwError $ SpecialFormErr "Unrecognized special form" v
