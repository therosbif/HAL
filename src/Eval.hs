{-# LANGUAGE LambdaCase #-}

module Eval where

import Ast (readExpr)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Data.Maybe (isNothing)
import Env (bindVars, defineVar, emptyEnv, getVar, setVar)
import Expr
    ( IOThrowsError,
      SchemeError(SpecialFormErr, Default, TypeMismatch, NumArgs,
                  NotFunction),
      Expr(List, DottedList, PrimitiveFunc, Func, String, Number, Bool,
           Atom, SpecicalFunc),
      Procedure,
      Env,
      liftThrows,
      SpecialForm )
import Procedures
    ( arithmeticProcedures, booleanProcedures, listProcedures )
import Utils (unpackList)

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
    ("lambda", lambda)
  ]

procedureBindings :: IO Env
procedureBindings =
  emptyEnv
    >>= flip bindVars (map createSpecial specialForms)
    >>= flip bindVars (map createPrimitives procedures)
  where
    createPrimitives (key, func) = (key, PrimitiveFunc func)
    createSpecial (key, func) = (key, SpecicalFunc func)

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

lambda :: SpecialForm
lambda env (List params : bo:dy) = makeNormalFunc env params (bo:dy)
lambda env (DottedList params varargs : bo:dy) =
  makeVarargs varargs env params (bo:dy)
lambda env (varargs@(Atom _) : bo:dy) = makeVarargs varargs env [] (bo:dy)
lambda _ (a : _) = throwError $ TypeMismatch "list" a
lambda _ a = throwError $ NumArgs 2 a

makeFunc :: Maybe String -> Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeFunc vaargs env params body =
  return $ Func (map show params) vaargs body env

makeNormalFunc :: Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeNormalFunc = makeFunc Nothing

makeVarargs :: Expr -> Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeVarargs = makeFunc . Just . show

--------------------------------------------------------------------------------

bindVarArgs :: Maybe String -> [Expr] -> Env -> IOThrowsError Env
bindVarArgs arg remainingArgs env =
  case arg of
    Nothing -> return env
    Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]

apply :: Expr -> [Expr] -> IOThrowsError Expr
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params vaarg body closure) args
  | num params /= num args && isNothing vaarg =
    throwError $ NumArgs (num params) args
  | otherwise =
    liftIO (bindVars closure $ zip params args)
      >>= bindVarArgs vaarg (drop (length params) args) >>= evalBody
        where
          num = toInteger . length
          evalBody env = last <$> mapM (eval env) body
apply f _ = throwError $ NotFunction "Function not found" (show f)

eval :: Env -> Expr -> IOThrowsError Expr
eval _ v@(String _) = return v
eval _ v@(Number _) = return v
eval _ v@(Bool _) = return v
eval env (Atom id) = getVar env id
eval env (List [v]) = eval env v
eval _ (List [Atom "quote", v]) = return v
eval env (List (func : args)) = eval env func >>= (\case
                  SpecicalFunc f -> f env args
                  f -> mapM (eval env) args >>= apply f)
eval _ v = throwError $ SpecialFormErr "Unrecognized special form" v
