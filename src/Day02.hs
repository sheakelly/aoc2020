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

type Parser = Parsec Void String

data Password = Password
  { min :: Int,
    max :: Int,
    required :: Char,
    value :: String
  }
  deriving (Show)

{--
11-17 l: llllllllllllllllll
-}
passwordParser :: Parser Password
passwordParser = do
  min <- decimal
  void $ char '-'
  max <- decimal
  void space
  required <- anySingle
  void $ char ':'
  void space
  value <- many letterChar
  void newline
  return $ Password min max required value

fileParser :: Parser [Password]
fileParser = do
  result <- many passwordParser
  return result

passwords :: String -> [Password]
passwords content =
  case result of
    Left _ -> []
    Right a -> a
  where
    result =
      runParser fileParser "" content

countChars :: String -> Char -> Int
countChars str c = length $ filter (== c) str

satisfiesPolicy1 :: Password -> Bool
satisfiesPolicy1 (Password {min, max, value, required}) =
  count >= min && count <= max
  where
    count = countChars value required

satisfiesPolicy2 :: Password -> Bool
satisfiesPolicy2 (Password {min, max, value, required}) =
  (pos1 == required && pos2 /= required)
    || (pos1 /= required && pos2 == required)
  where
    indices = findIndices (== required) value
    pos1 = value !! (min - 1)
    pos2 = value !! (max - 1)

part01 :: String -> Int
part01 content =
  passwords content
    & filter satisfiesPolicy1
    & length

part02 :: String -> Int
part02 content =
  passwords content
    & filter satisfiesPolicy2
    & length

main :: IO ()
main = do
  content <- readFile "./src/Day02.txt"
  print $ part01 content
  print $ part02 content
