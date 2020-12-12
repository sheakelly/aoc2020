{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))
import Data.List hiding (drop, take)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace

type ParentBags = Map Text [Text]

type ChildBags = Map Text [(Integer, Text)]

addParentBag :: [Text] -> ParentBags -> ParentBags
addParentBag [] bags = bags
addParentBag [_] bags = bags
addParentBag (outer : inners) bags =
  foldl
    (\bags' inner -> Map.insertWith (++) (T.drop 2 inner) [outer] bags')
    bags
    inners

findParentBags :: Text -> [Text] -> ParentBags -> [Text]
findParentBags color result bags =
  case outers of
    Nothing -> result
    Just x -> nub $ result ++ x ++ foldl (\acc x' -> findParentBags x' acc bags) result x
  where
    outers = Map.lookup color bags

asCountAndColor :: Text -> (Integer, Text)
asCountAndColor value =
  (read $ T.unpack $ T.take 2 value, T.drop 2 value)

addChildBag :: [Text] -> ChildBags -> ChildBags
addChildBag [] bags = bags
addChildBag [_] bags = bags
addChildBag (parent : children) bags =
  foldl
    (\bags' child -> Map.insertWith (++) parent [asCountAndColor child] bags')
    bags
    children

countChildBags :: Text -> ChildBags -> Integer
countChildBags color bags =
  case traceShow parent parent of
    Nothing -> 0
    Just children ->
      sum $
        map
          (\x -> fst x + (fst x * countChildBags (snd x) bags))
          children
  where
    parent = Map.lookup (traceShow color color) bags

parseInput :: String -> [[Text]]
parseInput input =
  T.pack input
    & T.replace " bags contain no other bags." ""
    & T.replace " bags contain " "|"
    & T.replace " bag, " "|"
    & T.replace " bags, " "|"
    & T.replace " bag." ""
    & T.replace " bags." ""
    & T.lines
    & map (T.splitOn "|")

part01 :: String -> Int
part01 input =
  parseInput input
    & foldl (flip addParentBag) Map.empty
    & findParentBags "shiny gold" []
    & length

part02 :: String -> Integer
part02 input =
  parseInput input
    & foldl (flip addChildBag) Map.empty
    & countChildBags "shiny gold"

main :: IO ()
main = do
  input <- readFile "./src/Day07.txt"
  print $ part01 input
  print $ part02 input
