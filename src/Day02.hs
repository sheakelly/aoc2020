{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (tails)
import Data.List.Split (splitOn)

data Password = Password
  { min_ :: Int,
    max_ :: Int,
    unknown :: String,
    value :: String
  }
  deriving (Show)

{--
11-17 l: llllllllllllllllll
-}
parsePassword :: String -> Password
parsePassword text =
  Password {min_ = 1, max_ = 2, unknown = unknown, value = value}
  where
    [pwLength, unknown, value] = words text
    [min_, max_] = splitOn "-" pwLength

isInvalid :: Password -> Bool
isInvalid password =
  length_ < (password . min_) || length_ > (password . max_)
  where
    length_ = length $ password . value

part01 :: String -> Int
part01 content =
  length $ filter . isInvalid $ map parsePassword $ lines content

main :: IO ()
main = do
  content <- readFile "./src/Day02.txt"
  print $ part01 content
