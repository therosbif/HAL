module Expr where

import Builtins (Procedure)
import Parser (Parser)
import Parsers (parseChar, padding, parseWord, parseDouble, parseInt)
import Control.Applicative ( Alternative(many, (<|>)) )

data Atom' =
    Integer Integer
  | Double Double
  | Word String

data Expr =
    Atom Atom'
  | List [Expr]

instance Show Expr where
  show (Atom (Integer x)) = "(Atom Integer " ++ show x ++ ")"
  show (Atom (Double x)) = "(Atom Double " ++ show x ++ ")"
  show (Atom (Word x)) = "(Atom Word " ++ x ++ ")"
  show (List x) = show x

ast :: Parser Char Expr
ast = expr
  where
    expr = padding (List <$> list <|> Atom <$> atom)
    list = parseChar '(' *> padding (many expr) <* parseChar ')'
    atom =
      Double <$> parseDouble <|>
      Integer <$> parseInt <|>
      Word <$> parseWord
