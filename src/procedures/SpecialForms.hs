module SpecialForms where

import Expr ( Expr(List, Procedure) )
import Error ( SchemeError(NumArgs), IOThrowsError )
import Env ( Env )
import Builtins (Procedure)
import Control.Monad.Except (MonadError(throwError))

type SpecialForm = Env -> [Expr] -> IOThrowsError Expr
