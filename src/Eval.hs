{-# LANGUAGE LambdaCase #-}
module Eval where
import Expr (Expr (String, Number, Double, Bool, List, Atom), ThrowsError, SchemeError (NotFunction, SpecialFormErr, Default, NumArgs, TypeMismatch), extractValue, showErr)
import Builtins (Procedure)
import Procedures (arithmeticProcedures, booleanProcedures, listProcedures, specialFormProcedures)
import Control.Monad.Except (MonadError(throwError))
import Utils (unpackList)
import Ast (readExpr)
import GHC.Exception (throw)

procedures :: [([Char], Procedure)]
procedures =
  [("cond", cond), ("if", schemeIf)]
  ++ arithmeticProcedures
  ++ booleanProcedures
  ++ listProcedures
  ++ specialFormProcedures

apply :: String -> Procedure
apply func args = maybe (throwError $ NotFunction
  "Unrecognised primitive function" func) ($ args) (lookup func procedures)

eval :: Expr -> ThrowsError Expr
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Double _) = return v
eval v@(Bool _)   = return v
eval (List [v])   = eval v
eval (List [Atom "quote", v]) = return v
eval (List (Atom func : args)) = mapM eval args >>= apply func . map unpackList
eval v = throwError $ SpecialFormErr "Unrecognized special form" v

cond :: Procedure
cond [] = throwError $ Default "No catch-all condition."
cond (List [pred, conseq]:xs) =
  eval pred >>= (\case
                    Bool False -> cond xs
                    Bool True -> eval conseq
                    v -> throwError $ TypeMismatch "bool" v)
cond (List e:_) = throwError $ NumArgs 2 e
cond (e:_) = throwError $ TypeMismatch "list" e

schemeIf :: Procedure
schemeIf [pred, conseq, alt] =
  eval pred >>= (\case
                  Bool True -> eval conseq
                  Bool False -> eval alt
                  v -> throwError $ TypeMismatch "boolean" v
                  )
schemeIf v = throwError $ NumArgs 3 v

interpret :: String -> IO ()
interpret s =
  let evaled = show <$> (readExpr s >>= eval)
  in putStrLn $ extractValue $ showErr evaled
