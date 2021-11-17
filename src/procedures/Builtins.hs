module Builtins where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map.Strict as Map
import Expr
  ( Expr (Atom, Bool, DottedList, List, Number, String, Procedure)
  )
import Utils (unpackBool, unpackList, unpackNum, unpackStr)
import Error (ThrowsError, SchemeError (NumArgs, TypeMismatch))
import Env (Env)

type Procedure = [Expr] -> ThrowsError Expr

----------------------------------------------------

binOp :: (Integer -> Integer -> Integer) -> Procedure
binOp _ v@[_] = throwError $ NumArgs 2 v
binOp op args
  | length args < 2 = throwError $ NumArgs 2 args
  | otherwise = Number . foldl1 op <$> mapM unpackNum args

boolBinOp :: (Expr -> Expr -> Bool) -> Procedure
boolBinOp op args
  | length args < 2 = throwError $ NumArgs 2 args
  | otherwise = return $ Bool $ pairs op args

pairs :: (a -> a -> Bool) -> [a] -> Bool
pairs f (x : y : t) = f x y && pairs f (y : t)
pairs f t = True

-- boolBinOp :: (Expr -> ThrowsError a) -> (a -> a -> Bool) -> Procedure
-- boolBinOp ext op args
--   | length args /= 2 = throwError $ NumArgs 2 args
--   | otherwise = do
--       l <- ext $ args !! 0
--       r <- ext $ args !! 1
--       return $ Bool $ l `op` r

-- numBoolBinOp :: (Integer -> Integer -> Bool) -> Procedure
-- numBoolBinOp  = boolBinOp (unpackNum . unpackList)

-- strBoolBinOp :: (String -> String -> Bool) -> Procedure
-- strBoolBinOp  = boolBinOp (unpackStr . unpackList)

-- boolBoolBinOp :: (Bool -> Bool -> Bool) -> Procedure
-- boolBoolBinOp = boolBinOp (unpackBool . unpackList)

----------------------------------------------------

equal :: Expr -> Expr -> Bool
equal a b = show a == show b

typeTest :: (Expr -> Bool) -> Procedure
typeTest c args = return $ Bool $ all c args

isAtom :: (Expr -> Bool)
isAtom (Atom _) = True
isAtom _ = False

isString :: (Expr -> Bool)
isString (String _) = True
isString _ = False

isNumber :: (Expr -> Bool)
isNumber (Number _) = True
isNumber _ = False

----------------------------------------------------

car :: Procedure
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [v] = throwError $ TypeMismatch "list" v
car v = throwError $ NumArgs 1 v

cdr :: Procedure
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] a] = return a
cdr [DottedList (_:xs) a] = return $ DottedList xs a
cdr [v] = throwError $ TypeMismatch "list" v
cdr v = throwError $ NumArgs 1 v

cons :: Procedure
cons [v, List []] = return $ List [v]
cons [v, List vs] = return $ List (v:vs)
cons [v, DottedList xs x] = return $ DottedList (v:xs) x
cons [x, y] = return $ DottedList [x] y
cons v = throwError $ NumArgs 2 v

----------------------------------------------------

lambda :: Procedure
lambda [args@(List _), e] = return $ Procedure args e
lambda [args, e] = return $ Procedure (List [args]) e
lambda v = throwError $ NumArgs 2 v
