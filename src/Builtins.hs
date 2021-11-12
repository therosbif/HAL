module Builtins where

import qualified Data.Map.Strict as Map
import Expr (Expr (Number, List))

data Procedure =
    Cons  | Car | Cdr                     -- List
  | Eq    | Atom                          -- Test
  | Add   | Sub | Mul | Div | Mod | Lt    -- Arithmetic
  | Quote | Lambda | Define | Let | Cond  -- Special forms
  | Custom String

procedures :: [([Char], Procedure)]
procedures =
  [("cons", Cons), ("car", Car), ("cdr", Cdr),
   ("eq?", Eq), ("atom?", Atom),
   ("+", Add), ("-", Sub), ("*", Mul), ("div", Div), ("mod", Mod), ("<", Lt),
   ("quote", Quote), ("lambda", Lambda), ("define", Define),
   ("let", Let), ("cond", Cond)]
