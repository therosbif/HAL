module Utils where
import Expr
    (Expr(Bool, List, Number, String, Func), ThrowsError, SchemeError (TypeMismatch), Env, IOThrowsError)
import Control.Monad.Except ( MonadError(throwError) )

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

makeFunc :: Maybe String -> Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeFunc vaargs env params body =
  return $ Func (map show params) vaargs body env

makeNormalFunc :: Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeNormalFunc = makeFunc Nothing

makeVarargs :: Expr -> Env -> [Expr] -> [Expr] -> IOThrowsError Expr
makeVarargs = makeFunc . Just . show
