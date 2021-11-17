module Expr where
import Parser (Parser)

data Expr =
    Atom String
  | List [Expr]
  | DottedList [Expr] Expr
  | Number Integer
  | String String
  | Bool Bool
  | Procedure Expr Expr

instance Show Expr where
  show (Atom x)     = x
  show (List x)     = "(" ++ (unwords . map show) x ++ ")"
  show (DottedList l e) =
    "(" ++ (unwords . map show) l ++ " . " ++ show e ++ ")"
  show (Number x)   = show x
  show (String x)   = "\"" ++ x ++ "\""
  show (Bool True)  = "#t"
  show (Bool False) = "#f"
  show (Procedure _ _) = "#<procedure>"

instance Eq Expr where
  (Atom a) == (Atom b)      = a == b
  (List []) == (List [])    = True
  (DottedList a (List [])) == b    = List a == b
  a == (DottedList b (List []))    = a == List b
  (Number a) == (Number b)  = a == b
  (String a) == (String b)  = a == b
  (Bool a) == (Bool b)      = a == b
  _ == _ = False

instance Ord Expr where
  compare (List a) (List b)     = compare a b
  compare (DottedList a as) b   = compare (List (a ++ [as])) b
  compare a (DottedList b bs)   = compare a (List (b ++ [bs]))
  compare (Number a) (Number b) = compare a b
  compare (String a) (String b) = compare a b
  compare (Bool a)   (Bool b)   = compare a b
  compare _ _ = GT
