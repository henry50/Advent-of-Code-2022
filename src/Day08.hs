module Day08 (runDay08) where

import Part ( Part(..) )
import Data.Char (digitToInt)
import Data.List (transpose)

data Tree = Tree { height :: Int, value :: Int} deriving Show

runDay08 :: Part -> [String] -> [String]
runDay08 p s = case p of
  One -> [show $ part1 s']
  Two -> [show $ part2 s']
  where s' = parseInput s

parseInput :: [String] -> [[Tree]]
parseInput = (map.map) (\x -> Tree (digitToInt x) 0)

part1 :: [[Tree]] -> Int
part1 = countVisible . processMap combineRows checkRow where
  countVisible :: [[Tree]] -> Int
  countVisible = sum . map (sum . map value)
  checkRow :: [Int] -> [Tree] -> [Tree]
  checkRow h (x:xs) = x {value = fromEnum $ height x > maximum h} : checkRow (height x: h) xs
  checkRow _ []     = []
  combineRows :: [Tree] -> [Tree] -> [Tree]
  combineRows = zipWith orTree where
    orTree :: Tree -> Tree -> Tree
    orTree t1 t2 = t1 { value = fromEnum $ value t1 + value t2 /= 0}

part2 :: [[Tree]] -> Int
part2 = maximum . map (maximum . map value) . processMap combineRows processRow where
  combineRows :: [Tree] -> [Tree] -> [Tree]
  combineRows = zipWith prodTree where
    prodTree :: Tree -> Tree -> Tree
    prodTree t1 t2 = t1 { value = value t1 * value t2 }
  viewDist :: Int -> [Int] -> Int
  viewDist h (x:xs)
    | h <= x = 1
    | h >  x = 1 + viewDist h xs
  viewDist _ _ = 0
  processRow :: [Int] -> [Tree] -> [Tree]
  processRow hs (x:xs) = case hs of
    [-1] -> x {value = 0} : processRow [height x] xs
    _ -> x { value = viewDist (height x) hs } : processRow (height x: hs) xs
  processRow _ [] = []

processMap :: ([Tree] -> [Tree] -> [Tree]) -> ([Int] -> [Tree] -> [Tree]) -> [[Tree]] -> [[Tree]]
processMap combineRows processRow ts = zipWith combineRows (processRows ts) (transpose $ processRows $ transpose ts) where
  processRows :: [[Tree]] -> [[Tree]]
  processRows = map (\x -> combineRows (processRow [-1] x) (reverse $ processRow [-1] (reverse x)))


