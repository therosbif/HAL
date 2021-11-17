module Utils where
import Expr
    ( Expr(Bool, List, Number, String))
import Control.Monad.Except ( MonadError(throwError) )
import Error (ThrowsError, SchemeError (TypeMismatch))

-----------------------------------------------

unpackList :: Expr -> Expr
unpackList (List [n]) = unpackList n
unpackList n = n

unpackNum :: Expr -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum nan = throwError $ TypeMismatch "number" nan

unpackStr :: Expr -> ThrowsError String
unpackStr (String s) = return s
unpackStr nan = throwError $ TypeMismatch "string" nan

unpackBool :: Expr -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool nan = throwError $ TypeMismatch "boolean" nan
