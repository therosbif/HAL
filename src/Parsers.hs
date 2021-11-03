module Parsers where

import Parser ( check, Parser(Success, Failure, Parser) )
import Control.Applicative ( Alternative(some, (<|>), many) )
import Control.Exception (catch)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isNothing)

parseChar :: Char -> Parser Char Char
parseChar c = check (== c)

parseCharStr :: Char -> Parser Char String
parseCharStr c = (:[]) <$> parseChar c

parseAnyOf :: String -> Parser Char Char
parseAnyOf s = check (`elem` s)

parseAnyNotOf :: String -> Parser Char Char
parseAnyNotOf s = check (`notElem` s)

padding :: Parser Char o -> Parser Char o
padding p = spaces *> p <* spaces
  where spaces = many $ parseAnyOf " \t\n\r"

parseInt :: Parser Char Integer
parseInt = read <$> number
  where
    number = sNumber <|> digits
    sNumber = (++) <$> parseCharStr '-' <*> digits
    digits = some $ parseAnyOf ['0'..'9']

parseDouble :: Parser Char Double
parseDouble = read <$> number
  where
    number = sNumber <|> decimal
    sNumber = (++) <$> parseCharStr '-' <*> decimal
    decimal = (+++) <$> digits <*> parseCharStr '.' <*> digits
    digits = some $ parseAnyOf ['0'..'9']
    (+++) a b c = a ++ b ++ c

parseWord :: Parser Char String
parseWord = some $ parseAnyNotOf invalidChars
  where
    invalidChars = [' ', '\n', '\t', '(', ')', ';']
