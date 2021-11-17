module Error where

import Control.Monad.Except
  (MonadError (catchError, throwError), ExceptT, runExceptT)
import Expr (Expr)
import Parser (Parser(Parser))

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

showErr :: (MonadError a m, Show a) => m String -> m String
showErr action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-------------------------------------------------------------

type IOThrowsError = ExceptT SchemeError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right v)  = return v

runIOThrows :: IOThrowsError String -> IO String
runIOThrows s = extractValue <$> runExceptT (showErr s)
