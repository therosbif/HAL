module Builtins where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map.Strict as Map
import Expr
  ( Expr (Atom, Bool, DottedList, Double, List, Number, String),
    SchemeError (NumArgs, TypeMismatch, Default),
    ThrowsError,
  )
import Utils (unpackBool, unpackList, unpackNum, unpackStr)

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
isNumber (Double _) = True
isNumber _ = False

----------------------------------------------------
