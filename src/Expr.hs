module Expr where
import Parser (Parser)
import Control.Monad.Except ( ExceptT, MonadError(..), runExceptT )
import Data.IORef (IORef)
import System.IO (Handle)

type Env = IORef [(String, IORef Expr)]
type Procedure = [Expr] -> ThrowsError Expr
type IOProcedure = [Expr] -> IOThrowsError Expr
type SpecialForm = Env -> [Expr] -> IOThrowsError Expr
type ThrowsError = Either SchemeError
type IOThrowsError = ExceptT SchemeError IO

data Expr =
    Atom String
  | List [Expr]
  | DottedList [Expr] Expr
  | Number Integer
  | String String
  | Bool Bool
  | Port Handle
  | PrimitiveFunc Procedure
  | SpecicalFunc SpecialForm
  | IOFunc IOProcedure
  | Func {  params :: [String],
            vaarg :: Maybe String,
            body  :: [Expr],
            closure :: Env }

instance Show Expr where
  show (Atom x)     = x
  show (List x)     = "(" ++ (unwords . map show) x ++ ")"
  show (DottedList l e) =
    "(" ++ (unwords . map show) l ++ " . " ++ show e ++ ")"
  show (Number x)   = show x
  show (String x)   = "\"" ++ x ++ "\""
  show (Bool True)  = "#t"
  show (Bool False) = "#f"
  show (Port _) = "#<IOport>"
  show (PrimitiveFunc _) = "#<primitive>"
  show (SpecicalFunc _) = "#<special>"
  show (IOFunc _) = "#<IOprimitive>"
  show (Func args vaargs _ _) =
    "(lambda (" ++ unwords (map show args) ++
      (case vaargs of
        Nothing -> ""
        Just a -> " . " ++ a) ++ ") ...)"

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

--------------------------------------------------------------------------------

data SchemeError =
    NumArgs Integer [Expr]
  | TypeMismatch String Expr
  | Parse (Parser Char Expr) String
  | SpecialFormErr String Expr
  | NotFunction String String
  | UnboundVar String String
  | Default String

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

-------------------------------------------------------------

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right v)  = return v
