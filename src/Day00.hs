module Day00 (runDay00) where
import Data.Char (toUpper)
import Part

{-
    Just a quick test program to check the main running program works
-}

runDay00 :: Part -> [String] -> [String]
runDay00 n = case n of
    One -> map reverse
    Two -> map.map $ toUpper