module JsonParser where

import Parser
import Data.Map (Map)
import Data.Char
import Control.Applicative (
  pure,
  empty,
  (<|>)
  )

itemP :: Parser Char
itemP = Parser $ \input ->
  case input of
    "" -> empty
    (c:cs) -> [(c, cs)]

charP :: Char -> Parser Char
charP p = do
  c <- itemP
  if c == p then return c else empty

stringP :: String -> Parser String
stringP "" = return ""
stringP (c:cs) = do { charP c
                    ; stringP cs
                    ; return (c:cs)}

notNullP :: Parser [a] -> Parser [a]
notNullP (Parser p) = Parser $ \input -> do
  (x, xs) <- p input
  if null x then empty else [(x, xs)]

spanP :: (Char -> Bool) -> Parser String
spanP p = notNullP (Parser f)
  where f input = [span p input]


data JsonValue  = JsonNull
                | JsonBool Bool
                | JsonNumber Integer
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject (Map String JsonValue)
                deriving (Show)


jsonNullP :: Parser JsonValue
jsonNullP = JsonNull <$ stringP "null"

jsonBoolP :: Parser JsonValue
jsonBoolP = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

jsonNumberP :: Parser JsonValue
jsonNumberP = f <$> spanP isDigit
  where f ds = JsonNumber $ read ds

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> (
  charP '"'
  *> spanP (/= '"')
  <* charP '"')

jsonArrayP :: Parser JsonValue
jsonArrayP = JsonArray <$> (
  charP '['
  *> jsonValueP
  <* charP ']')

jsonObjectP :: Parser JsonValue
jsonObjectP = JsonObject <$> (
  charP '{'
  *> jsonValueP
  <* charP '}')

jsonValueP :: Parser JsonValue
jsonValueP
  = jsonNullP
  <|> jsonBoolP
  <|> jsonNumberP
  <|> jsonStringP
  <|> jsonArrayP
  <|> jsonObjectP
