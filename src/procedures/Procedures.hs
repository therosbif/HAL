module Procedures where
import Builtins
    ( binOp,
      boolBinOp,
      isAtom,
      isNumber,
      isString,
      typeTest,
      Procedure, car, cdr, cons, equal )
import Control.Monad.Except (MonadError(throwError))

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
    ("equal?", boolBinOp equal),
    ("<", boolBinOp (<)),
    (">", boolBinOp (>)),
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
