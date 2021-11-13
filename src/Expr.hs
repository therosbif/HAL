module Expr where
import Parser (Parser)
import Control.Monad.Except (MonadError (catchError))

data Expr =
    Atom String
  | List [Expr]
  | DottedList [Expr] Expr
  | Number Integer
  | Double Double
  | String String
  | Bool Bool
  | Error String

instance Show Expr where
  show (Atom x) = x
  show (List x) = "(" ++ (unwords . map show) x ++ ")"
  show (DottedList l e) =
    "(" ++ (unwords . map show) l ++ " . " ++ show e ++ ")"
  show (Number x) = show x
  show (Double x) = show x
  show (String x) = "\"" ++ x ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Error x) = "Error: " ++ x

instance Eq Expr where
  (Atom a) == (Atom b)      = a == b
  (List a) == (List b)      = a == b
  (DottedList a as) == b    = List (a ++ [as]) == b
  a == (DottedList b bs)    = a == List (b ++ [bs])
  (Number a) == (Number b)  = a == b
  (Double a) == (Double b)  = a == b
  (Number a) == b           = Double (fromIntegral a) == b
  a == (Number b)           = a == Double (fromIntegral b)
  (String a) == (String b)  = a == b
  (Bool a) == (Bool b)      = a == b
  _ == _ = False

instance Ord Expr where
  compare (List a) (List b)     = compare a b
  compare (DottedList a as) b   = compare (List (a ++ [as])) b
  compare a (DottedList b bs)   = compare a (List (b ++ [bs]))
  compare (Number a) (Number b) = compare a b
  compare (Double a) (Double b) = compare a b
  compare (Number a) b          = compare (Double $ fromIntegral a) b
  compare a          (Number b) = compare a (Double $ fromIntegral b)
  compare (String a) (String b) = compare a b
  compare (Bool a)   (Bool b)   = compare a b
  compare _ _ = GT

-------------------------------------------------------------

data SchemeError =
    NumArgs Integer [Expr]
  | TypeMismatch String Expr
  | Parse (Parser Char Expr) String
  | SpecialFormErr String Expr
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either SchemeError

instance Show SchemeError where
  show (UnboundVar msg varname)   = msg ++ ": " ++ varname
  show (SpecialFormErr msg form)  = msg ++ ": " ++ show form
  show (NotFunction msg func)     = msg ++ ": " ++ show func
  show (Default msg)              = msg
  show (Parse err errpos)         =
    "Parse error: " ++ show err ++ "\n" ++ errpos
  show (NumArgs expected found)   =
    "Expected " ++ show expected ++ " args. Found values: " ++
    (unwords . map show) found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found

showErr :: ThrowsError String -> ThrowsError String
showErr action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
