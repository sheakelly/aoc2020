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

move :: Int -> Int -> String -> Coord -> Coord
move right down row (Coord {x, y}) =
  Coord {x = adjusted, y = y + down}
  where
    x' = x + right
    adjusted = if x' < length row then x' else x' - length row

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

isTreeAt :: Int -> String -> Bool
isTreeAt i row = isTree $ row !! i

traverseMap :: String -> Slope -> Int
traverseMap content (Slope {down, right}) =
  foldl
    ( \(coord@(Coord {x, y}), trees) row ->
        ( move right down row coord,
          if isTreeAt x row
            then trees + 1
            else trees
        )
    )
    (Coord 0 0, 0)
    map
    & snd
  where
    map = lines content

part01 :: String -> Int
part01 content =
  traverseMap content (Slope 3 1)

part02 :: String -> Int
part02 content =
  traverseMap content <$> [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
    & product

main :: IO ()
main = do
  content <- readFile "./src/Day03.txt"
  print $ part01 content
  print $ part02 content
