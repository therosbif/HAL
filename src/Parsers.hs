module Parsers where

import Parser ( Parser(Success, Failure, Parser), (<+>) )
import Control.Applicative ( Alternative(some, (<|>), many) )
import Control.Exception (catch)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isNothing)
import Data.Functor ( ($>) )
import Expr (Expr(Atom, Double, Number, Bool, String))

check :: (i -> Bool) -> Parser i i
check f = Parser $ \s -> case s of
  (x:xs) | f x -> Success x xs
  _            -> Failure "Invalid char." s

char :: Char -> Parser Char Char
char c = check (== c)

charStr :: Char -> Parser Char String
charStr c = (:[]) <$> char c

anyOf :: String -> Parser Char Char
anyOf s = check (`elem` s)

anyNotOf :: String -> Parser Char Char
anyNotOf s = check (`notElem` s)

------------------------------------------

padding :: Parser Char o -> Parser Char o
padding p = spaces *> p <* spaces
  where spaces = many $ anyOf " \t\n\r"

parens :: Parser Char o -> Parser Char o
parens p = padding (char '(' *> p <* char ')')

------------------------------------------

-- TODO (rosbif): parse numbers in different bases and data sizes
int :: Parser Char Integer
int = read <$> number
  where
    number = sNumber <|> digits
    sNumber = charStr '-' <+> digits
    digits = some $ anyOf ['0'..'9']

double :: Parser Char Double
double = read <$> number
  where
    number = sNumber <|> decimal
    sNumber = charStr '-' <+> decimal
    decimal = digits <+> charStr '.' <+> digits
    digits = some $ anyOf ['0'..'9']

bool :: Parser Char Bool
bool = keyWord "#t" $> True <|> keyWord "#f" $> False

string :: Parser Char String
string = char '"' *> stringContent <* char '"'
  where
    stringContent = many (escaped <|> anyNotOf "\"")
    escaped = quote <|> backslash <|> newline <|> tab <|> carriageReturn
      where
        quote = char '\\' *> char '"'
        backslash = char '\\' *> char '\\'
        newline = keyWord "\\n" $> '\n'
        tab = keyWord "\\t" $> '\t'
        carriageReturn = keyWord "\\r" $> '\r'

-- TODO (rosbif): implement character litteral parsing
atom :: Parser Char String
atom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit <|> char '@')
  return $ first : rest
    where
      letter = anyOf $ ['a'..'z'] ++ ['A'..'Z']
      symbol = anyOf ['?', '!', '+', '-', '*', '/', '<', '=', '>',
                      ':', '$', '%', '^', '&', '_', '~']
      digit = anyOf "1234567890"

value :: Parser Char Expr
value =
  Double  <$> double  <|>
  Number  <$> int     <|>
  Bool    <$> bool    <|>
  String  <$> string  <|>
  Atom    <$> atom

keyWord :: String -> Parser Char String
keyWord = foldr (\ x -> (<*>) ((:) <$> char x)) (pure [])
