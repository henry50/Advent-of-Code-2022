module Day04 (runDay04) where

import Part ( Part(..) )

import Data.List.Split ( splitOn )


runDay04 :: Part -> [String] -> [String]
runDay04 p s = case p of
  One -> [show $ solve s' fullyContains]
  Two -> [show $ solve s' anyContains]
  where s' = parseInput s

solve :: [((Int, Int), (Int, Int))] -> ((Int, Int) -> (Int, Int) -> Bool) -> Int
solve s f = length.filter (== True) $ map (uncurry f) s

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (a, b) (c, d) = ((c >= a) && (d <= b)) || ((a >= c) && (b <= d))

anyContains :: (Int, Int) -> (Int, Int) -> Bool
anyContains (a, b) (c, d) = ((a <= c) && (c <= b)) ||
                            ((a <= d) && (d <= b)) ||
                            ((c <= a) && (a <= d)) ||
                            ((c <= b) && (b <= d))

parseInput :: [String] -> [((Int, Int), (Int, Int))]
parseInput = map splitInput where
  listToPair :: [a] -> (a,a)
  listToPair [a,b] = (a,b)
  listToPair _     = error "Not a pair"
  rangeToPair :: String -> (Int, Int)
  rangeToPair x = (read a, read b) where
    (a, b) = listToPair $ splitOn "-" x
  splitInput :: String -> ((Int, Int), (Int, Int))
  splitInput x = (rangeToPair a, rangeToPair b) where
    (a, b) = listToPair $ splitOn "," x
