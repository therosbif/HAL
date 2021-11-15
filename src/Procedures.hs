module Procedures where
import Expr (Expr, ThrowsError, SchemeError (NotFunction))
import Builtins
    ( binOp,
      boolBinOp,
      isAtom,
      isNumber,
      isString,
      typeTest,
      Procedure )
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
    ("<", boolBinOp (<)),
    (">", boolBinOp (>)),
    ("atom?", typeTest isAtom),
    ("string?", typeTest isString),
    ("number?", typeTest isNumber)
  ]
