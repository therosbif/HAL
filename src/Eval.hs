module Eval where
import Expr (Expr (String, Number, Double, Bool, List, Atom), ThrowsError, SchemeError (NotFunction, SpecialFormErr, Default, NumArgs, TypeMismatch), extractValue, showErr)
import Builtins (Procedure)
import Procedures (arithmeticProcedures, booleanProcedures)
import Control.Monad.Except (MonadError(throwError))
import Utils (unpackList)
import Ast (readExpr)

procedures :: [([Char], Procedure)]
procedures = [("cond", cond)] ++ arithmeticProcedures ++ booleanProcedures

apply :: String -> Procedure
apply func args = maybe (throwError $ NotFunction
  "Unrecognised primitive function" func) ($ args) (lookup func procedures)

eval :: Expr -> ThrowsError Expr
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Double _) = return v
eval v@(Bool _)   = return v
eval (List [v]) = eval v
eval (List [Atom "quote", v]) = return v
eval (List (Atom func : args)) = mapM eval args >>= apply func . map unpackList
eval v = throwError $ SpecialFormErr "Unrecognized special form" v

cond :: Procedure
cond [] = throwError $ Default "No catch-all condition."
cond (List [pred, conseq]:xs) =
  eval pred >>= (\e -> case e of
                    (Bool False) -> cond xs
                    _ -> eval conseq)
cond (List e:_) = throwError $ NumArgs 2 e
cond (e:_) = throwError $ TypeMismatch "list" e

interpret :: String -> IO ()
interpret s =
  let evaled = show <$> (readExpr s >>= eval)
  in putStrLn $ extractValue $ showErr evaled
