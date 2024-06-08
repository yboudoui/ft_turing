module Parser where

import Control.Applicative

newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> [(f x, input') | (x, input') <- p input]

instance Applicative Parser where
  -- pure  :: a -> f a
  pure a = Parser $ \x -> [(a, x)]
  -- (<*>) :: f (a -> b) -> f a -> f b
  (Parser p) <*> (Parser f) = Parser $ \input -> do
    (v, cs) <- p input
    (v', cs') <- f cs
    [(v v', cs')]

instance Monad Parser where
  -- m a -> (a -> m b) -> m b
  p >>= f = Parser $ \cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs]

instance Alternative Parser where
  -- empty :: f a
  empty = Parser $ \_ -> []
  -- (<|>) :: f a -> f a -> f a
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input
