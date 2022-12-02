module Day02 (runDay02) where

import Part ( Part(..) )


runDay02 :: Part -> [String] -> [String]
runDay02 p s = case p of
  One -> [show $ part1 s]
  Two -> [show $ part2 s]


splitLine :: String -> (Char, Char)
splitLine x = (head x, x !! 2)

part1 :: [String] -> Int
part1 s = sum $ map outcome s where
  outcome :: String -> Int
  outcome x = case splitLine x of
    ('A','X') -> 1 + 3 
    ('A','Y') -> 2 + 6
    ('A','Z') -> 3 + 0
    ('B','X') -> 1 + 0
    ('B','Y') -> 2 + 3
    ('B','Z') -> 3 + 6
    ('C','X') -> 1 + 6
    ('C','Y') -> 2 + 0
    ('C','Z') -> 3 + 3
    _         -> error "Invalid pair"

part2 :: [String] -> Int
part2 s = sum $ map outcome s where
  outcome :: String -> Int
  outcome x = case splitLine x of
    ('A','X') -> 3 + 0
    ('A','Y') -> 1 + 3
    ('A','Z') -> 2 + 6
    ('B','X') -> 1 + 0
    ('B','Y') -> 2 + 3
    ('B','Z') -> 3 + 6
    ('C','X') -> 2 + 0
    ('C','Y') -> 3 + 3
    ('C','Z') -> 1 + 6
    _         -> error "Invalid pair"