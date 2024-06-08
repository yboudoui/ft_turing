module JsonParser where

import Parser
import Data.Map (Map)
import Data.Char
import Control.Applicative (
  pure,
  empty,
  (<|>),
  many,
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

whiteSpace :: Parser String
whiteSpace = spanP isSpace

token :: Parser String -> Parser String
token p = whiteSpace *> p <* whiteSpace

symbol :: String -> Parser String
symbol str = token (stringP str)

stringLiteralP :: Parser String
stringLiteralP = symbol "\"" *> spanP (/= '"') <* symbol "\""

seperateBy :: Parser a -> Parser b -> Parser [a]
seperateBy a b = many (a <* b)

seperateBySymbol :: Parser a -> String -> Parser [a]
seperateBySymbol a str = a `seperateBy` symbol str

type JsonProperty = (String,JsonValue)

data JsonValue  = JsonNull
                | JsonBool Bool
                | JsonNumber Integer
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [JsonProperty]
                deriving (Show)


jsonNullP :: Parser JsonValue
jsonNullP = JsonNull <$ symbol "null"

jsonBoolP :: Parser JsonValue
jsonBoolP = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ symbol "true"
    jsonFalse = JsonBool False <$ symbol "false"

jsonNumberP :: Parser JsonValue
jsonNumberP = f <$> spanP isDigit
  where f ds = JsonNumber $ read ds

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> stringLiteralP

jsonArrayP :: Parser JsonValue
jsonArrayP = JsonArray <$> (symbol "[" *> (jsonValueP `seperateBySymbol` ",") <* symbol "]")

jsonPropertyP :: Parser JsonProperty
jsonPropertyP = liftA2 (,) (stringLiteralP <* symbol ":") jsonValueP

jsonObjectP :: Parser JsonValue
jsonObjectP = JsonObject <$> (symbol "{" *> (jsonPropertyP `seperateBySymbol` ",") <* symbol "}")

jsonValueP :: Parser JsonValue
jsonValueP = jsonNullP
  <|> jsonBoolP
  <|> jsonNumberP
  <|> jsonStringP
--  <|> jsonArrayP
--  <|> jsonObjectP
