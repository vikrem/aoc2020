{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Functor ((<&>))
import Data.List (intersect, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T (getContents)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parse, sepBy, someTill)
import Text.Megaparsec.Char (letterChar, newline)

type Parser = Parsec Void Text

main :: IO ()
main = do
  stdin <- T.getContents
  case parse (groups <* eof) "stdin" stdin of
    Left err -> print err
    Right input -> do
      print $ run1 input
      print $ run2 input

line :: Parser Text
line = T.pack <$> someTill letterChar newline

groups :: Parser [[Text]]
groups = sepBy (many line) newline

countUniq :: [Text] -> Int
countUniq xs = length $ nub $ concat $ xs <&> T.unpack

countIntersect :: [Text] -> Int
countIntersect xs = length $ foldl1 intersect $ xs <&> T.unpack

run1 :: [[Text]] -> Int
run1 xxs = sum $ xxs <&> countUniq

run2 :: [[Text]] -> Int
run2 xxs = sum $ xxs <&> countIntersect