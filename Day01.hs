module Main where

import Data.List (tails)

stringToInt :: String -> Int
stringToInt text =
  read text :: Int

parseFile :: FilePath -> IO [Int]
parseFile filePath = do
  contents <- readFile filePath
  return $ map stringToInt (lines contents)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys | y : xs' <- tails xs, ys <- combinations (n - 1) xs'
  ]

findValues :: Int -> [Int] -> [Int]
findValues size values =
  foldl
    ( \acc group ->
        if sum group == 2020
          then product group : acc
          else acc
    )
    []
    combos
  where
    combos = combinations size values

main :: IO ()
main = do
  values <- parseFile "input01.txt"
  print $ findValues 2 values
  print $ findValues 3 values
