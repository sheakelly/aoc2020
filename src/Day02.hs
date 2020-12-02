{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (tails)
import Data.List.Split (splitOn)

data Password = Password
  { minimum :: Int,
    maximum :: Int,
    unknown :: String,
    value :: String
  }
  deriving (Show)

{--
11-17 l: llllllllllllllllll
-}
parsePassword :: String -> Password
parsePassword text =
  Password {minimum = 1, maximum = 2, unknown = unknown, value = value}
  where
    [pwLength, unknown, value] = words text
    [min, max] = splitOn "-" pwLength

isInvalid :: Password -> Bool
isInvalid password =
  pwLength < password . minimum || pwLwbLength > password . maximum

part01 :: String -> Int
part01 content =
  length $ filter . isInvalid $ map parsePassword $ lines content

main :: IO ()
main = do
  content <- readFile "./src/Day02.txt"
  print $ part01 content
