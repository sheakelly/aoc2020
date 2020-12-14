{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Function ((&))
import Data.List
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Op = Nop | Acc | Jmp deriving (Show, Eq)

parseOp :: Parser Op
parseOp =
  choice
    [ Nop <$ string "nop",
      Acc <$ string "acc",
      Jmp <$ string "jmp"
    ]

type Inst = (Op, Int)

data Memory = Memory
  { program :: [Inst],
    pointer :: Int,
    acc :: Int,
    visited :: [Int],
    exited :: Bool
  }
  deriving (Show)

parseInst :: Parser Inst
parseInst = do
  op <- parseOp
  _ <- spaceChar
  arg <- L.signed space L.decimal
  return (op, arg)

parseProgram :: Parser [Inst]
parseProgram = do
  inst <- parseInst `endBy` newline
  eof
  return inst

compute :: Inst -> Memory -> Memory
compute inst memory@Memory {pointer, acc, visited} =
  case inst of
    (Acc, value) -> memory {acc = acc + value, pointer = pointer + 1, visited = pointer : visited}
    (Jmp, value) -> memory {pointer = pointer + value, visited = pointer : visited}
    (Nop, _) -> memory {pointer = pointer + 1, visited = pointer : visited}

runProgram :: Memory -> Memory
runProgram memory@Memory {program, pointer, visited}
  | pointer `elem` visited = memory
  | pointer >= length program = memory {exited = True}
  | otherwise =
    runProgram $ compute inst memory
  where
    inst = program !! pointer

loadMemory :: String -> Maybe Memory
loadMemory input =
  case runParser parseProgram "" input of
    Left err -> traceShow (errorBundlePretty err) Nothing
    Right program ->
      Just $ Memory program 0 0 [] False

part01 :: String -> Int
part01 input =
  case loadMemory input of
    Just memory -> acc $ runProgram memory
    Nothing -> 0

{--
 - This is a inefficient way to do this, looks a Seq in future
 -}
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b)
  where
    (a, _ : b) = splitAt n ls

swapInstAt :: Int -> Memory -> Memory
swapInstAt pos memory@Memory {program} =
  case program !! pos of
    (Jmp, value) -> memory {program = replaceAtIndex pos (Nop, value) program}
    (Nop, value) -> memory {program = replaceAtIndex pos (Jmp, value) program}
    _ -> memory

part02 :: String -> Int
part02 input =
  case loadMemory input of
    Just memory ->
      program memory
        & findIndices (\a -> fst a == Jmp || fst a == Nop)
        & map (`swapInstAt` memory)
        & map runProgram
        & filter exited
        & head
        & acc
    Nothing -> 0

main :: IO ()
main = do
  input <- readFile "./src/Day08.txt"
  print $ part01 input
  print $ part02 input
