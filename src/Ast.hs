module Ast where

import Control.Applicative (Alternative (empty, many), (<|>))
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Expr
  ( Expr
      ( Atom,
        DottedList,
        List
      ),
    IOThrowsError,
    SchemeError (Parse),
    ThrowsError,
    liftThrows,
  )
import Parser (Parser (Failure, Parser), runParser)
import Parsers (anyNotOf, char, padding, parens, value)

ast :: Parser Char Expr
ast = expr
  where
    expr = padding (quoted <|> value <|> parens (dottedList <|> list))
    quoted = char '\'' >> expr >>= (\x -> return $ List [Atom "quote", x])
    dottedList = do
      head <- many expr <* padding (char '.')
      DottedList head <$> expr
    list = List <$> many expr

readOrThrow :: Parser Char a -> String -> ThrowsError a
readOrThrow p s = case runParser p s of
  Left (e, i) -> throwError $ Parse (Failure e i) (errpos s i)
  Right o -> return o
  where
    errpos s i =
      let pos = length s - length i
          begin = take pos s
          end = drop pos s
          red = "\x1b[31m"
          reset = "\x1b[0m"
       in "\t" ++ begin ++ red ++ "here-->" ++ reset ++ end ++ "\n"

readExpr :: String -> ThrowsError Expr
readExpr = readOrThrow ast

readExprList :: String -> ThrowsError [Expr]
readExprList =
  readOrThrow (many $ padding ast)
