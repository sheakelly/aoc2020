{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
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
keep 'L' xs = fst $ splitAt (length xs `div` 2) xs
keep 'R' xs = snd $ splitAt (length xs `div` 2) xs
keep _ xs = xs

row :: String -> Int
row seat =
  head $ foldl (\acc cur -> keep cur acc) [0 .. 127] (take 7 seat)

column :: String -> Int
column seat =
  head $ foldl (\acc cur -> keep cur acc) [0 .. 7] (drop 7 seat)

calcSeatId :: (Int, Int) -> Int
calcSeatId (row, column) = row * 8 + column

calcSeatIds :: String -> [Int]
calcSeatIds input =
  map (liftA2 (,) row column) seats
    & map calcSeatId
  where
    seats = lines input

part01 :: String -> Int
part01 input =
  maximum $ calcSeatIds input

part02 :: String -> Int
part02 input =
  head $ [minimum seatIds .. maximum seatIds] \\ sort seatIds
  where
    seatIds = calcSeatIds input

main :: IO ()
main = do
  input <- readFile "./src/Day05.txt"
  print $ part01 input
  print $ part02 input
