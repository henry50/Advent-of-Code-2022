{-# LANGUAGE InstanceSigs #-}
module Day11 (runDay11) where

import Part ( Part(..) )
import Data.List (stripPrefix, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Map (Map, (!))
import qualified Data.Map as M

data Monkey = Monkey {
  items :: Seq Integer,
  operation :: Integer -> Integer,
  testDiv :: Integer,
  testTrue :: Int,
  testFalse :: Int,
  inspectCount :: Integer
}

runDay11 :: Part -> [String] -> [String]
runDay11 p s = case p of
  One -> [show $ solve 20 (`div` 3) s']
  Two -> [show $ solve 10000 id s']
  where s' = parseInput s

parseInput :: [String] -> Map Int Monkey
parseInput x = M.fromList $ zip [0..length x - 1] $ parseInput' x where
  parseInput' [] = []
  parseInput' y = parseMonkey (take 6 y) : parseInput' (drop 7 y)
  parseMonkey :: [String] -> Monkey
  parseMonkey [_, s, o, t, tr, f] = Monkey {
    items = S.fromList $ map read $ splitOn ", " $ fromJust $ stripPrefix "  Starting items: " s,
    operation = case words $ fromJust $ stripPrefix "  Operation: new = old " o of
      ["+", "old"] -> (* 2)
      ["*", "old"] -> (^ (2 :: Integer))
      ["+", n]     -> (+ read n)
      ["*", n]     -> (* read n)
      _            -> error "Could not parse operation",
    testDiv = read $ fromJust $ stripPrefix "  Test: divisible by " t,
    testTrue = read $ fromJust $ stripPrefix "    If true: throw to monkey " tr,
    testFalse = read $ fromJust $ stripPrefix "    If false: throw to monkey " f,
    inspectCount = 0
  }
  parseMonkey _ = error "Monkey does not match expected format"
--
solve :: Int -> (Integer -> Integer) -> Map Int Monkey -> Integer
solve rounds worryOp mapping =  product . take 2 . sortOn negate . map inspectCount . M.elems . (!! rounds) . iterate doRound $ mapping where
  commonDiv :: Integer
  commonDiv = product $ map testDiv $ M.elems mapping
  doRound :: Map Int Monkey -> Map Int Monkey
  doRound ms = foldl (flip doTurn) ms (M.keys ms)
  doTurn :: Int -> Map Int Monkey -> Map Int Monkey
  doTurn m ms = if null itms then ms else ms'' where
    itms = items (ms ! m)
    ms' = M.adjust (\x -> x { items = S.empty }) m ms
    ms'' = foldl (inspect m) ms' itms
  inspect :: Int -> Map Int Monkey -> Integer -> Map Int Monkey
  inspect m ms i = M.adjust (\x -> x { items = items (ms ! newMonkeyIdx) S.|> newWorry } ) newMonkeyIdx ms'  where
    m' = ms ! m
    ms' = M.adjust (\x -> x { inspectCount = inspectCount x + 1 }) m ms
    newWorry = worryOp (operation m' i) `mod` commonDiv
    newMonkeyIdx = if newWorry `mod` testDiv m' == 0 then testTrue m' else testFalse m'