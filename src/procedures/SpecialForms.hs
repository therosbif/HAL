module SpecialForms where

import Control.Monad.Except (MonadError (throwError))
import Expr
  ( Expr (Atom, DottedList, List),
    SchemeError (NumArgs, TypeMismatch),
    SpecialForm,
  )
import Utils (makeNormalFunc, makeVarargs)

lambda :: SpecialForm
lambda env (List params : bo : dy) = makeNormalFunc env params (bo : dy)
lambda env (DottedList params varargs : bo : dy) =
  makeVarargs varargs env params (bo : dy)
lambda env (varargs@(Atom _) : bo : dy) = makeVarargs varargs env [] (bo : dy)
lambda _ (a : _) = throwError $ TypeMismatch "list" a
lambda _ a = throwError $ NumArgs 2 a
