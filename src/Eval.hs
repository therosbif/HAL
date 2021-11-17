{-# LANGUAGE LambdaCase #-}

module Eval where

import Ast (readExpr)
import Builtins (Procedure, lambda)
import Control.Monad.Except (MonadError (throwError))
import Env (Env, getVar, defineVar, setVar)
import Error
  ( IOThrowsError,
    SchemeError (Default, NotFunction, NumArgs, SpecialFormErr, TypeMismatch),
    liftThrows,
  )
import Expr (Expr (Atom, Bool, List, Number, String, Procedure))
import GHC.Exception (throw)
import Procedures
  ( arithmeticProcedures,
    booleanProcedures,
    listProcedures
  )
import Utils (unpackList)
import SpecialForms (SpecialForm)

procedures :: [([Char], Procedure)]
procedures =
  [ ("lambda", lambda) ]
  ++ arithmeticProcedures
  ++ booleanProcedures
  ++ listProcedures

specialForms :: [(String, SpecialForm)]
specialForms =
  [ ("cond", cond),
    ("if", schemeIf),
    ("define", define),
    ("set!", set)
  ]

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
define _   [key, _]        = throwError $ TypeMismatch "atom" key
define _   args            = throwError $ NumArgs 2 args

set :: SpecialForm
set    env [Atom key, val] = eval env val >>= setVar env key
set    _   [key, _]        = throwError $ TypeMismatch "atom" key
set    _   args            = throwError $ NumArgs 2 args

-- schemeLet :: SpecialForm
-- schemeLet env (List [key, val]:xs) =
--------------------------------------------------------------------------------

apply :: String -> Procedure
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognised primitive function" func)
    ($ args)
    (lookup func procedures)

eval :: Env -> Expr -> IOThrowsError Expr
eval _   v@(String _) = return v
eval _   v@(Number _) = return v
eval _   v@(Bool _) = return v
eval env (Atom id) = getVar env id
eval env (List [v]) = eval env v
eval _   (List [Atom "quote", v]) = return v
eval env (List (Atom func : args)) = case lookup func specialForms of
  Just f -> f env args
  Nothing -> mapM (eval env) args >>= liftThrows . apply func . map unpackList
eval _ v = throwError $ SpecialFormErr "Unrecognized special form" v
