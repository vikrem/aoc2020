{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (join)
import Data.Functor ((<&>))
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T (getContents)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, manyTill, parse, sepBy, (<|>))
import Text.Megaparsec.Char (char, letterChar, space, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Bag = Bag
  { quality :: Text,
    colour :: Text
  }
  deriving (Show, Eq, Ord)

data Rule = Rule
  { srcBag :: Bag,
    subBags :: [(Int, Bag)]
  }
  deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

main :: IO ()
main = do
  stdin <- T.getContents
  case parse (many parseRule) "stdin" stdin of
    Left err -> print err
    Right rules -> do
      print $ run1 rules
      print $ run2 rules

dual :: Rule -> [Rule]
dual Rule {..} = subBags <&> \b -> Rule {srcBag = snd b, subBags = [(1, srcBag)]}

mapping :: Rule -> M.Map Bag [(Int, Bag)]
mapping Rule {..} = M.singleton srcBag subBags

run1 :: [Rule] -> Int
run1 rs =
  let join a b = nub $ a ++ b
      mapped = (rs >>= dual) <&> mapping
      merged = M.unionsWith join mapped
      children = dfs (Bag "shiny" "gold") merged
      out = length children
   in out

run2 :: [Rule] -> Int
run2 rs =
  let join a b = nub $ a ++ b
      mapped = rs <&> mapping
      merged = M.unionsWith join mapped
      w = width (Bag "shiny" "gold") merged
   in w - 1

dfs :: Ord a => a -> M.Map a [(Int, a)] -> [a]
dfs here m = case M.lookup here m of
  Nothing -> []
  Just xs -> nub $ (snd <$> xs) ++ join (xs <&> (`dfs` m) . snd)

width :: Bag -> M.Map Bag [(Int, Bag)] -> Int
width here m = case M.lookup here m of
  Just xs -> 1 + sum (xs <&> \(count, next) -> count * width next m)
  Nothing -> 0

parseBag :: Parser Bag
parseBag = do
  quality <- T.pack <$> manyTill letterChar spaceChar
  colour <- T.pack <$> manyTill letterChar spaceChar
  string "bags" <|> string "bag"
  space
  pure Bag {..}

parseQuantBag :: Parser (Int, Bag)
parseQuantBag = do
  count <- decimal
  space
  bag <- parseBag
  pure (count, bag)

parseNoSubBag :: Parser [a]
parseNoSubBag = do
  string "no other bags"
  pure []

parseRule :: Parser Rule
parseRule = do
  srcBag <- parseBag
  string "contain"
  space
  subBags <- parseNoSubBag <|> sepBy parseQuantBag (string ", ")
  char '.'
  spaceChar
  return Rule {..}
