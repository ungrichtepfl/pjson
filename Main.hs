{-# OPTIONS_GHC -Wall -O2 #-}

module Main where

import           Control.Applicative (Alternative (empty, many, (<|>)))
import           Data.Char           (isDigit, isSpace)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure, exitSuccess)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floating point numbers
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

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

jsonBool :: Parser JsonValue
jsonBool = JsonBool True <$ stringP "true" <|> JsonBool False <$ stringP "false"

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (spanP isDigit)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy sep jsonValue
    sep = ws *> charP ',' <* ws

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> pairs <* ws <* charP '}')
  where
    pairs :: Parser [(String, JsonValue)]
    pairs = sepBy (ws *> charP ',' <* ws) pair
    pair :: Parser (String, JsonValue)
    pair = Parser $ \input -> do
      (rest, s) <- runParser stringLiteral input
      (rest', _) <- runParser (ws *> charP ':' <* ws) rest
      (rest'', jv) <- runParser jsonValue rest'
      return (rest'', (s, jv))

--  NOTE: Pair can also be implemented as:
--    pair =
--      (\key _ value -> (key, value)) <$> stringLiteral
--        <*> (ws *> charP ':' <* ws)
--        <*> jsonValue

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (rest, x) <- p input
  if null x
    then Nothing
    else Just (rest, x)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (x, rest) = span f input
   in Just (rest, x)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

-- NOTE: no escape support
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP -- same as sequenceA . map charP

jsonValue :: Parser JsonValue
jsonValue = ws *> (jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject) <* ws

parseFile :: Parser a -> FilePath -> IO (Maybe a)
parseFile p fp = do
  content <- readFile fp
  return $ snd <$> runParser p content

parseJsonFile :: FilePath -> IO (Maybe JsonValue)
parseJsonFile = parseFile jsonValue

main :: IO ()
main = getArgs >>= parse >>= parseJsonFile >>= print

parse :: [String] -> IO String
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse [fileName] = return fileName
parse _ = usage >> putStrLn "\nWrong kind of arguments" >> exitFailure

usage :: IO ()
usage = putStrLn "Usage: pjson filename"

version :: IO ()
version = putStrLn "Haskell pjson 0.1"
