module Ast where
import Parser (Parser(Parser))
import Expr (Expr (List, Double, Number, Bool, String, Atom, DottedList))
import Parsers (padding, double, int, bool, string, atom, char, value, parens)
import Control.Applicative (Alternative(many), (<|>))

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

-- eval :: Expr -> Either (String, Expr) (String, Int)
-- eval
