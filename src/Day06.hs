{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Function ((&))
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple
import Data.Void (Void)
import Debug.Trace
import Prelude hiding (lines, max, min)

splitInput :: String -> [[String]]
splitInput input = splitOn "\n\n" input & map (splitOn "\n")

part01 :: String -> Int
part01 input =
  splitInput input
    & map concat
    & map nub
    & map length
    & sum

part02 :: String -> Int
part02 input =
  splitInput input
    & map (\group -> (length (filter (not . null) group), group))
    & map (\(count, xs) -> (count, sort $ concat xs))
    & map (\(count, xs) -> (count, groupBy (==) xs))
    & filter (\(count, xs) -> any (\xs' -> length xs' == count) xs)
    & map (\(count, xs) -> foldl (\acc x -> if length x == count then acc + 1 else acc) 0 xs)
    & sum

main :: IO ()
main = do
  input <- readFile "./src/Day06.txt"
  print $ part01 input
  print $ part02 input
