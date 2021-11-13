module Ast where
import Parser (Parser(Parser, Failure), runParser)
import Expr (Expr (List, Double, Number, Bool, String, Atom, DottedList, Error), ThrowsError, SchemeError (Parse, SpecialFormErr, NotFunction), extractValue, showErr)
import Parsers (padding, double, int, bool, string, atom, char, value, parens)
import Control.Applicative (Alternative(many), (<|>))
import Builtins (procedures)
import Control.Monad.Except (MonadError(throwError))

apply :: String -> [Expr] -> ThrowsError Expr
apply func args = maybe (throwError $ NotFunction
  "Unrecognised primitive function args" func) ($ args) (lookup func procedures)

eval :: Expr -> ThrowsError Expr
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Double _) = return v
eval v@(Bool _)   = return v
eval (List [Atom "quote", v]) = return v
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval v = throwError $ SpecialFormErr "Unrecognized special form" v

ast :: Parser Char Expr
ast = expr
  where
    expr = padding (quoted <|> value <|> parens (dottedList <|> list))
    quoted = char '\'' >> expr >>= (\x -> return $ List [Atom "quote", x])
    dottedList = do
      head <- many expr <* padding (char '.')
      tail <- padding expr
      return $ DottedList head tail
    list = List <$> many expr

readExpr :: String -> ThrowsError Expr
readExpr s = case runParser ast s of
  Left (e,i) -> throwError $ Parse (Failure e i) (errpos s i)
  Right o -> return o
  where
    errpos s i = let
      pos   = length s - length i
      begin = take pos s
      end   = drop pos s
      in "\t" ++ begin ++ " <here> " ++ end ++ "\n"

interpret :: String -> IO ()
interpret s =
  let evaled = show <$> (readExpr s >>= eval)
  in putStrLn $ extractValue $ showErr evaled
