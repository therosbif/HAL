module Ast where
import Parser (Parser(Parser, Failure), runParser)
import Expr (Expr (List, Atom, DottedList))
import Parsers (padding, char, value, parens)
import Control.Applicative (Alternative(many), (<|>))
import Control.Monad.Except (MonadError(throwError))
import Error (ThrowsError, SchemeError (Parse))

ast :: Parser Char Expr
ast = expr
  where
    expr = padding (quoted <|> value <|> parens (dottedList <|> list))
    quoted = char '\'' >> expr >>= (\x -> return $ List [Atom "quote", x])
    dottedList = do
      head <- many expr <* padding (char '.')
      DottedList head <$> expr
    list = List <$> many expr

readExpr :: String -> ThrowsError Expr
readExpr s = case runParser ast s of
  Left (e,i) -> throwError $ Parse (Failure e i) (errpos s i)
  Right o -> return o
  where errpos s i = let
          pos   = length s - length i
          begin = take pos s
          end   = drop pos s
          red   = "\x1b[31m"
          reset = "\x1b[0m"
          in "\t" ++ begin ++ red ++ "here-->" ++ reset ++ end ++ "\n"
