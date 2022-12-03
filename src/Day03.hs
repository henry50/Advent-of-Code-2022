module Day03 (runDay03) where

import Part ( Part(..) )
import Data.List (intersect)
import Data.Char (ord, isAsciiLower, isAsciiUpper)

runDay03 :: Part -> [String] -> [String]
runDay03 p s = case p of
  One -> [show $ part1 s]
  Two -> [show $ part2 s]

priority :: Char -> Int
priority c
  | isAsciiLower c = ord c - 96 -- a = 97 becomes 1
  | isAsciiUpper c = ord c - 38 -- A = 65 becomes 27
  | otherwise = error "Invalid character encountered"

part1 :: [String] -> Int
part1 = sum . map (priority . repeated) where
  repeated :: String -> Char
  repeated s = head $ uncurry intersect $ splitAt (length s `div` 2) s

part2 :: [String] -> Int
part2 = sum . map priority . groups where
  groups :: [String] -> [Char]
  groups (x:y:z:xs) = head (x `intersect` (y `intersect` z)) : groups xs
  groups _ = []