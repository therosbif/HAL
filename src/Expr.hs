module Expr where

data Expr =
    Atom String
  | List [Expr]
  | DottedList [Expr] Expr
  | Number Integer
  | Double Double
  | String String
  | Bool Bool

instance Show Expr where
  show (Atom x) = x
  show (Number x) = show x
  show (Double x) = show x
  show (String x) = "\"" ++ x ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List x) = "(" ++ (unwords . map show) x ++ ")"
  show (DottedList l e) =
    "(" ++ (unwords . map show) l ++ " . " ++ show e ++ ")"
