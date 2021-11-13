module Builtins where

import qualified Data.Map.Strict as Map
import Expr (Expr (Number, List, Error, String, Bool, Double, Atom, DottedList), ThrowsError, SchemeError (NumArgs, TypeMismatch))
import Utils (pairs)
import Control.Monad.Except (MonadError(throwError))

type Procedure = [Expr] -> ThrowsError Expr
-- data Procedure' =
--     Cons  | Car | Cdr                     -- List
--   | Eq                             -- Test
--   | Lt    -- Arithmetic
--   | Lambda | Define | Let | Cond  -- Special forms
--   | Custom String

procedures :: [([Char], Procedure)]
procedures =
  [("eq?", binComp (==)), ("<", binComp (<)),
   ("atom?", typeTest isAtom), ("string?", typeTest isString),
   ("number?", typeTest isNumber),
   ("+", binOp (+)), ("-", binOp (-)), ("*", binOp (*)),
   ("div", binOp div), ("mod", binOp mod),
   ("quotient", binOp quot), ("remainder", binOp rem)]

----------------------------------------------------

binComp :: (Expr -> Expr -> Bool) -> Procedure
binComp _ v@[_] = throwError $ NumArgs 2 v
binComp op args = Bool . pairs op <$> mapM unpackList args

typeTest :: (Expr -> Bool) -> Procedure
typeTest c args = Bool . all c <$> mapM unpackList args

isAtom :: (Expr -> Bool)
isAtom (Atom _) = True
isAtom  _       = False

isString :: (Expr -> Bool)
isString (String _) = True
isString  _         = False

isNumber :: (Expr -> Bool)
isNumber (Number _) = True
isNumber (Double _) = True
isNumber  _         = False

----------------------------------------------------

binOp :: (Integer -> Integer -> Integer) -> Procedure
binOp _ v@[_] = throwError $ NumArgs 2 v
binOp op args = Number . foldl1 op <$> mapM unpack args
  where
    unpack a = unpackList a >>= unpackNum

unpackNum :: Expr -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum nan = throwError $ TypeMismatch "number" nan

unpackList :: Expr -> ThrowsError Expr
unpackList (List [n]) = unpackList n
unpackList n = return n
