{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Function ((&))
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (pack, stripSuffix, unpack)
import Data.Tuple
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Debug
import Text.Regex.TDFA
import Prelude hiding (lines, max, min)

type Parser = Parsec Void String

type Field = (String, String)

tuplify2 :: [a] -> Maybe (a, a)
tuplify2 [] = Nothing
tuplify2 [x] = Nothing
tuplify2 [x, y] = Just (x, y)

parseFields :: [String] -> [Field]
parseFields fields =
  map tuplify2 fields' & catMaybes
  where
    fields' = splitOn ":" <$> fields

parseInput :: String -> [[Field]]
parseInput input =
  splitOn "\n\n" input
    & map (splitOneOf "\n ")
    & map parseFields

hasField :: String -> [Field] -> Bool
hasField key fields =
  isJust $ lookup key fields

hasRequiredFields :: [Field] -> Bool
hasRequiredFields passport =
  if length passport == 8
    then True
    else
      if length passport == 7
        then not $ hasField "cid" passport
        else False

hasValidFields :: [Field] -> Bool
hasValidFields =
  all isFieldValid

validateHeight :: String -> Bool
validateHeight value
  | "cm" `isSuffixOf` value == True =
    case stripSuffix "cm" (pack value) of
      Just noSuffix ->
        unpack noSuffix
          & read
          & (\h -> h >= 150 && h <= 193)
      Nothing -> False
  | "in" `isSuffixOf` value == True =
    case stripSuffix "in" (pack value) of
      Just noSuffix ->
        unpack noSuffix
          & read
          & (\h -> h >= 59 && h <= 76)
      Nothing -> False
  | otherwise = False

isFieldValid :: Field -> Bool
isFieldValid ("byr", value) =
  value' >= 1920 && value' <= 2002
  where
    value' = read value
isFieldValid ("iyr", value) =
  value' >= 2010 && value' <= 2020
  where
    value' = read value
isFieldValid ("eyr", value) =
  value' >= 2020 && value' <= 2030
  where
    value' = read value
isFieldValid ("hgt", value) = validateHeight value
isFieldValid ("hcl", value) = value =~ ("#[0-9a-f]{6}" :: String) :: Bool
isFieldValid ("ecl", value) = elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid ("pid", value) = value =~ ("[0-9]{9}" :: String) :: Bool
isFieldValid (_, _) = True

part01 :: String -> Int
part01 input =
  length $ filter hasRequiredFields passports
  where
    passports = parseInput input

part02 :: String -> Int
part02 input =
  length $ filter hasValidFields $ filter hasRequiredFields passports
  where
    passports = parseInput input

main :: IO ()
main = do
  input <- readFile "./src/Day04.txt"
  print $ part01 input
  print $ part02 input

{-print $ part01 input-}
