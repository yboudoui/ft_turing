module Parser where

import Control.Monad

newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> [(f x, input') | (x, input') <- p input]

instance Applicative Parser where
  pure a = Parser $ \x -> [(a, x)]
  (Parser p) <*> (Parser f) = Parser $ \input -> do
    (v, cs) <- p input
    (v', cs') <- f cs
    [(v v', cs')]

instance Monad Parser where
  p >>= f = Parser $ \cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs]


instance MonadPlus Parser where
  mzero = Parser $ \cs -> []
