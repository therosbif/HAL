module Parser where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad ( ap, (>=>) )
data Parser i o
  = Success o [i]
  | Failure String [i]
  | Parser
      {parse :: [i] -> Parser i o}

instance (Eq i, Eq o) => Eq (Parser i o) where
  (Success o1 i1) == (Success o2 i2) = o1 == o2 && i1 == i2
  (Failure _ _) == (Failure _ _) = True
  (Parser _) == (Parser _) = True
  _ == _ = False
instance (Show i, Show o) => Show (Parser i o) where
  show (Success o i) = "Success (" ++ show o ++ ", " ++ show i ++ ")"
  show (Failure e i) = "Failure (\"" ++ e ++ "\", " ++ show i ++ ")"
  show (Parser f) = "Parser"

instance Functor (Parser i) where
  fmap f (Success o i) = Success (f o) i
  fmap _ (Failure e i) = Failure e i
  fmap f (Parser g) = Parser $ fmap f.g

instance Applicative (Parser i) where
  pure x = Parser $ Success x
  (<*>) = ap

instance Monad (Parser i) where
  (Failure e i) >>= _ = Failure e i
  (Parser g) >>= f = Parser $ g >=> f
  (Success o i) >>= f = case f o of
                          Parser p -> p i
                          o -> o

instance Alternative (Parser i) where
  empty = Parser $ \i -> Failure "" i
  (Failure e i) <|> p2 = p2
  (Parser f) <|> (Parser g) = Parser $ \s -> f s <|> g s
  (Parser f) <|> p2 = Parser $ \s -> f s <|> p2
  l <|> _  = l

check :: (i -> Bool) -> Parser i i
check f = Parser $ \s -> case s of
  (x:xs) | f x -> Success x xs
  _            -> Failure "Predicate failed" s

runParser :: Parser i o -> [i] -> Either (String,[i]) o
runParser (Success o i) s = Right o
runParser (Failure e i) s = Left (e,i)
runParser (Parser f) s = case f s of
  Success o [] -> Right o
  Success o i  -> Left  ("Stream was not entirely consumed.", i)
  Failure e i  -> Left  (e,i)
  Parser g     -> runParser (g s) s
