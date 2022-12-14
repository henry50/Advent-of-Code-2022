module Day01 (runDay01) where

import Part ( Part(..) )
import Data.List (sort)

runDay01 :: Part -> [String] -> [String]
runDay01 p = case p of
  One -> part1
  Two -> part2

parseInput' :: [String] -> [[String]]
parseInput' xs = case break (== "") xs of
    (a, _:b) -> a : parseInput' b
    (a, [])  -> [a]

parseInput :: [String] -> [[Int]]
parseInput xs = (map.map) (\x -> read x :: Int) $ parseInput' xs

part1 :: [String] -> [String]
part1 s = [show maxm] where
    maxm = maximum $ map sum $ parseInput s
    
part2 :: [String] -> [String]
part2 s = [show top3] where
    top3 = sum $ take 3 $ reverse $ sort $ map sum $ parseInput s