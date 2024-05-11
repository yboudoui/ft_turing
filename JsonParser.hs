module JsonParser where

import Parser

import Data.Map

data JsonValue  = JsonBool Bool
                | JsonNumber Integer
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject (Map String JsonValue)

itemP :: Parser Char
itemP = Parser $ \input ->
  case input of
    "" -> []
    (c: cs) -> [(c, cs)]

charP :: Char -> Parser Char
charP p = Parser f
  where f input
          | (c == p) = return c
          | otherwise = []
          where c = itemP input
          --where c = itemP input


stringP :: String -> Parser String
stringP "" = return ""
stringP (c:cs) = do { charP c
                                  ; stringP cs
                                  ; return (c:cs)}


--stringParser :: String -> Parser String
--stringParser = undefined
--
--parseBool :: Parser JsonValue
--parseBool = jsonTrue <|> jsonFalse
--  where
--    jsonTrue = JsonBool True <$ stringP "true"
--    jsonFalse = JsonBool False <$ stringP "false"
--
--instance MonadZero Parser where
--zero = Parser (\cs -> [])
--
--instance MonadPlus Parser where
--p ++ q = Parser (\cs -> parse p cs ++ parse q cs)
