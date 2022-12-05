module Day05 (runDay05) where

import Part ( Part(..) )
import Data.List (transpose)
import Data.Sequence as S (fromList, Seq (Empty), take, drop, index, update, (><), lookup, reverse)
import Data.Foldable (Foldable(toList))
import Data.Maybe (fromMaybe)
import Data.Function ((&))


runDay05 :: Part -> [String] -> [String]
runDay05 p s = case p of
  One -> [solve stacks moves S.reverse]
  Two -> [solve stacks moves id]
  where
    (a, b) = splitInput s
    stacks = parseStacks a
    moves  = parseMoves b

solve :: Seq (Seq Char) -> [(Int, Int, Int)] -> (Seq Char -> Seq Char) -> String
solve stacks moves modifier = toList $ getHeads finalStacks where
  getHeads :: Seq(Seq Char) -> String
  getHeads l = toList $ fromMaybe Empty $ mapM (S.lookup 0) l
  finalStacks :: Seq(Seq Char)
  finalStacks = foldl (&) stacks moveAppliers
  moveAppliers :: Seq(Seq (Seq Char) -> Seq (Seq Char))
  moveAppliers = applyMove <$> fromList moves
  applyMove :: (Int, Int, Int) -> Seq (Seq Char) -> Seq (Seq Char)
  applyMove (num, stFrom, stTo) st = st'' where
    -- Get items to move, either reverse for stack order (part 1) or don't (part 2)
    moved = modifier $ S.take num (st `index` stFrom)
    -- Drop items from origin stack
    st' = update stFrom (S.drop num (st `index` stFrom)) st
    -- Add items to destination stack
    st'' = update stTo (moved >< (st `index` stTo)) st'



splitInput :: [String] -> ([String], [String])
splitInput s = (a, Prelude.drop 1 b) where
  (a, b) = break (== "") s

parseStacks :: [String] -> Seq (Seq Char)
parseStacks s  = fromList . map (fromList . dropWhile (== ' ') . init) $ transpose $ parseStacks' s where
  parseStacks' (x:xs) =  parseRow x : parseStacks' xs where
    parseRow :: String -> [Char]
    parseRow (_:y:_:' ':ys) = y : parseRow ys
    parseRow (_:y:_:ys)     = y : parseRow ys
    parseRow _ = []
  parseStacks' _ = []

parseMoves :: [String] -> [(Int, Int, Int)]
parseMoves = map parseMove where
  parseMove :: String -> (Int, Int, Int)
  -- -1 for zero indexing
  parseMove s = (a, b-1, c-1) where
    w = words s
    -- move
    a = read $ w !! 1
    -- from
    b = read $ w !! 3
    -- to
    c = read $ w !! 5