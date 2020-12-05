{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Function ((&))
import Data.List
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Debug
import Prelude hiding (lines, max, min)

data Coord = Coord {x :: Int, y :: Int} deriving (Show)

data Slope = Slope {right :: Int, down :: Int}

move :: Slope -> String -> Coord -> Coord
move (Slope {right, down}) row (Coord {x, y}) =
  Coord {x = adjusted, y = y + down}
  where
    x' = x + right
    adjusted = if x' < length row then x' else x' - length row

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

isTreeAt :: Int -> String -> Bool
isTreeAt i row = isTree $ row !! i

traverseMap' :: Coord -> [String] -> Int -> Slope -> Int
traverseMap' coord@(Coord {x, y}) map trees slope
  | y > length map - 1 = trees
  | otherwise = traverseMap' coord' map trees' slope
  where
    row = map !! y
    coord' = move slope row coord
    trees' =
      if isTreeAt x row
        then trees + 1
        else trees

traverseMap :: String -> Slope -> Int
traverseMap input slope =
  traverseMap' (Coord 0 0) (lines input) 0 slope

part01 :: String -> Int
part01 input =
  traverseMap input (Slope 3 1)

part02 :: String -> Int
part02 input =
  traverseMap input <$> [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
    & product

main :: IO ()
main = do
  input <- readFile "./src/Day03.txt"
  print $ part01 input
  print $ part02 input
