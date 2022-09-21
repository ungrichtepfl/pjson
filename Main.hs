module Main where

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumer Integer -- NOTE: no support for floating point numbers
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- NOTE: No proper error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (rest, x) <- p input
      return (rest, f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (rest1, f) <- p1 input
      (rest2, x) <- p2 rest1
      return (rest2, f x)

jsonNull :: Parser JsonValue
jsonNull = undefined

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined
