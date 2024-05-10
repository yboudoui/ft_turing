module Main where

newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
return a = Parser (\cs -> [(a,cs)])
p >>= f = Parser (\cs -> concat [parse (f a) cs’ | (a,cs’) <- parse p cs])

instance MonadZero Parser where
zero = Parser (\cs -> [])

instance MonadPlus Parser where
p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

item :: String -> [(Char, String)]
item "" = []
item (c:cs) = [(c, cs)]

parseItem :: Parser Char
parseItem = Parser (item)

newtype CharacterPredicate = Char -> Bool

makeParserCharacterPredicate :: CharacterPredicate -> Parser Char
makeParserCharacterPredicate p = do {c <- item; if p c then return c else zero}

makeParserCharacterMatch :: Char -> Parser Char
makeParserCharacterMatch c = makeParserCharacterPredicate (c ==)

makeParserStringMatch :: String -> Parser String
makeParserStringMatch "" = return ""
makeParserStringMatch (c:cs) = do { makeParserCharacterMatch c
                                  ; makeParserStringMatch cs
                                  ; return (c:cs)}

main :: IO ()
main = putStrLn "Hello, Haskell!"
