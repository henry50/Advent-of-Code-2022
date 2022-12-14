module Day10 (runDay10) where

import Part ( Part(..) )
import Data.List.Split (chunksOf)


runDay10 :: Part -> [String] -> [String]
runDay10 p s = case p of
  One -> [show $ part1 s']
  Two -> chunksOf 40 $ part2 s'
  where s' = parseInstructions s

parseInstructions :: [String] -> [Int]
parseInstructions = scanl1 (+) . go 1 where
  go :: Int -> [String] -> [Int]
  go _ []     = []
  go p (x:xs) = case words x of
    ["noop"]    -> p : go 0 xs
    ["addx", n] -> p : 0 : go (read n) xs
    _           -> error "Could not parse input"

part1 :: [Int] -> Int
part1 x = sum $ map (\y -> y * (x !! (y-1))) [20,60..220]

part2 :: [Int] -> [Char]
part2 x = map draw [0..239] where
  draw :: Int -> Char
  draw n = if abs (s - c) <= 1 then '█' else ' ' where s = x !! n; c = n `mod` 40