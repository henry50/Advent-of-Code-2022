module Day06 (runDay06) where

import Part ( Part(..) )


runDay06 :: Part -> [String] -> [String]
runDay06 p s = case p of
  One -> [show $ solve 4 $ head s]
  Two -> [show $ solve 14 $ head s]

solve :: Int -> String -> Int
solve len s = r [] s 0 where
  unique :: Eq a => [a] -> Bool
  unique [] = True
  unique (x:xs) = x `notElem` xs && unique xs
  r :: [Char] -> [Char] -> Int -> Int
  r y (x:xs) n
    | length y == len = if unique y then n else r (tail y ++ [x]) xs (n+1)
    | otherwise = r (y ++ [x]) xs (n+1)
  r _ _ _ = -1 -- the match has not been found
