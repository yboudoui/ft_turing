module Main where

import System.Environment
import System.IO (
  Handle,
  IOMode(ReadMode),
  withFile,
  hGetContents
  )
import Data.Map (Map)

import Control.Monad (Monad)

import Parser

data JsonValue  = JsonBool Bool
                | JsonNumber Integer
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject (Map String JsonValue)


--stringParser :: String -> Parser String
--stringParser = undefined
--
--parseBool :: Parser JsonValue
--parseBool = jsonTrue <|> jsonFalse
--  where
--    jsonTrue = JsonBool True <$ stringP "true"
--    jsonFalse = JsonBool False <$ stringP "false"
--

--useFile :: (String -> IO()) -> Handle -> IO()
--useFile = \func handle -> do
--  text <- hGetContents handle
--  func text



newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Monad Parser where
return a = Parser (\cs -> [(a,cs)])
p >>= f = Parser (\cs -> concat [parse (f a) cs'
                                | (a,cs') <- parse p cs])

--instance MonadZero Parser where
--zero = Parser (\cs -> [])
--
--instance MonadPlus Parser where
--p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

item :: String -> [(Char, String)]
item "" = []
item (c:cs) = [(c, cs)]

parseItem :: Parser Char
parseItem = Parser (item)

type CharacterPredicate = Char -> Bool

--makeParserCharacterPredicate :: CharacterPredicate -> Parser Char
--makeParserCharacterPredicate p = do {c <- item; if p c then return c else zero}
--
--makeParserCharacterMatch :: Char -> Parser Char
--makeParserCharacterMatch c = makeParserCharacterPredicate (c ==)

--makeParserStringMatch :: String -> Parser String
--makeParserStringMatch "" = return ""
--makeParserStringMatch (c:cs) = do { makeParserCharacterMatch c
--                                  ; makeParserStringMatch cs
--                                  ; return (c:cs)}

main :: IO()
main = do
  (fileName:_) <- getArgs
  withFile fileName ReadMode (useFile print)
