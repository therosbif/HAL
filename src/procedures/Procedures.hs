module Procedures where

import Builtins
  ( binOp,
    boolBinOp,
    car,
    cdr,
    cons,
    equal,
    isAtom,
    isNumber,
    isString,
    typeTest, logicOp
  )
import Control.Monad.Except (MonadError (throwError))
import Expr (Procedure)

arithmeticProcedures :: [([Char], Procedure)]
arithmeticProcedures =
  [ ("+", binOp (+)),
    ("-", binOp (-)),
    ("*", binOp (*)),
    ("div", binOp div),
    ("mod", binOp mod),
    ("quotient", binOp quot),
    ("remainder", binOp rem)
  ]

booleanProcedures :: [([Char], Procedure)]
booleanProcedures =
  [ ("eq?", boolBinOp (==)),
    ("=", boolBinOp (==)),
    ("equal?", boolBinOp equal),
    ("<", boolBinOp (<)),
    (">", boolBinOp (>)),
    ("&&", logicOp (&&)),
    ("||", logicOp (||)),
    ("atom?", typeTest isAtom),
    ("string?", typeTest isString),
    ("number?", typeTest isNumber)
  ]

listProcedures :: [([Char], Procedure)]
listProcedures =
  [ ("car", car),
    ("cdr", cdr),
    ("cons", cons)
  ]
