module Builtins where

data ListOp = Cons | Car | Cdr
data CondOp = Eq | Atom
data ArithmeticOp = Add | Sub | Mul | Div | Mod | Lt
data SpecialOp = Quote | Lambda | Define

data Procedure =
    ListOp
  | CondOp
  | ArithmeticOp
  | SpecialOp
