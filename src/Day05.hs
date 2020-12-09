{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Function ((&))
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Void (Void)
import Debug.Trace
import Prelude hiding (lines, max, min)

keep :: Char -> [Int] -> [Int]
keep 'F' xs = fst $ splitAt (length xs `div` 2) xs
keep 'B' xs = snd $ splitAt (length xs `div` 2) xs
keep _ xs = xs

row :: String -> Int
row seat =
  foldl (\acc cur -> keep cur acc) rows (take 7 seat) !! 0
  where
    rows = [0 .. 127]

part01 :: String -> [Int]
part01 input =
  map row seats
  where
    seats = lines input

part02 :: String -> Int
part02 input = 0

main :: IO ()
main = do
  input <- readFile "./src/Day05.txt"
  print $ part01 input
  print $ part02 input

{-print $ part01 input-}
