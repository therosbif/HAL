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
  show (Atom (Integer x)) = "(Integer " ++ show x ++ ")"
  show (Atom (Double x)) = "(Double " ++ show x ++ ")"
  show (Atom (Word x)) = "(Word " ++ x ++ ")"
  show (List x) = show x

ast :: Parser Char Expr
ast = List <$> list
  where
    expr = padding (List <$> list <|> Atom <$> atom)
    list = parens $ padding (many expr)
    atom =
      Double <$> parseDouble <|>
      Integer <$> parseInt <|>
      Word <$> parseWord
    parens p = padding (parseChar '(' *> p <* parseChar ')')
