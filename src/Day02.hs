{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T (getContents)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, anySingle, many, parse)
import Text.Megaparsec.Byte.Lexer (lexeme)
import Text.Megaparsec.Char (char, letterChar, space, space1)
import Text.Megaparsec.Char.Lexer (decimal)

data Entry = Entry
  { lowerBound :: Int,
    upperBound :: Int,
    letter :: Char,
    string :: Text
  }
  deriving (Show)

type Parser = Parsec Void Text

main :: IO ()
main = do
  stdin <- T.getContents
  print $ run1 stdin
  print $ run2 stdin

parser :: Parser Entry
parser = do
  low <- decimal
  char '-'
  high <- decimal
  space
  c <- anySingle
  char ':'
  space
  s <- many letterChar
  pure $ Entry low high c (T.pack s)

isValid :: Entry -> Bool
isValid Entry {..} = lowerBound <= c && c <= upperBound
  where
    c = T.count (T.singleton letter) string

isValid2 :: Entry -> Bool
isValid2 Entry {..} = neq && eq
  where
    fst = T.index string $ lowerBound - 1
    snd = T.index string $ upperBound - 1
    neq = fst /= snd
    eq = fst == letter || snd == letter

runParse :: Text -> Either (ParseErrorBundle Text Void) [Entry]
runParse = parse (many $ lexeme space1 parser) "input"

parseAndFilter :: Text -> (Entry -> Bool) -> Either (ParseErrorBundle Text Void) Int
parseAndFilter input f = length . filter f <$> runParse input

run1 :: Text -> Either (ParseErrorBundle Text Void) Int
run1 input = parseAndFilter input isValid

run2 :: Text -> Either (ParseErrorBundle Text Void) Int
run2 input = parseAndFilter input isValid2