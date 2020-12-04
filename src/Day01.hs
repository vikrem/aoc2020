module Main where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Control.Monad.State.Strict (evalState,  forM, modify, State, get )
import Data.Foldable (asum)

main :: IO ()
main = do
    stdin <- getContents
    print $ run1 $ parseInput stdin
    print $ run2 $ parseInput stdin

parseInput :: String -> [Int]
parseInput = mapMaybe readMaybe . lines

run1 :: [Int] -> Maybe (Int, Int)
run1 xs = 
    let
        loop = forM xs $ step1 2020 -- iterate over the whole input
        search = asum <$> loop -- grab the first match found, if any
        exec = evalState search IS.empty -- start with an empty set
    in
        exec

run2 :: [Int] -> Maybe (Int, Int, Int)
run2 xs = 
    let 
        loop = forM xs step2
        search = asum <$> loop
        exec = evalState search IS.empty
    in
        exec

step1 :: Int -> Int -> State IntSet (Maybe (Int, Int))
step1 target i = do
    set <- get -- grab the set so far
    let other = target-i
    if IS.member other set -- search for target - i
    then do
        pure $ Just (other, i) -- found it
    else do
        modify $ IS.insert i -- nope. insert i into the set
        pure Nothing

step2 :: Int -> State IntSet (Maybe (Int, Int, Int))
step2 i = do
    set <- IS.toList <$> get -- grab the set as a list
    let other = 2020 - i
    inner <- asum <$> forM set (step1 other) -- find two numbers that sum to 2020 - i
    case inner of
        Just (x, y) -> do -- found it
            pure $ Just (x, y, i)
        Nothing -> do -- nope. insert i into the set
            modify $ IS.insert i
            pure Nothing
