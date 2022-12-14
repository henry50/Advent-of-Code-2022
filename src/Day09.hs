{-# LANGUAGE InstanceSigs #-}
module Day09 (runDay09) where

import Part ( Part(..) )

import Prelude hiding (Either(..)) -- Hides normal Left and Right definitions
import Data.Set (Set)
import qualified Data.Set as S

data Instruction = Up Int | Down Int | Left Int | Right Int
data Point = Point { xCoord :: Int, yCoord :: Int } deriving (Eq, Ord)

instance Show Point where
  show :: Point -> String
  show p = "(" ++ show (xCoord p) ++ "," ++ show (yCoord p) ++ ")"

runDay09 :: Part -> [String] -> [String]
runDay09 p s = case p of
  One -> [show $ solve 2 s']
  Two -> [show $ solve 10 s']
  where s' = parseInstructions s

parseInstructions :: [String] -> [Instruction]
parseInstructions = map (\x -> case words x of
  ["U", i] -> Up $ read i
  ["D", i] -> Down $ read i
  ["L", i] -> Left $ read i
  ["R", i] -> Right $ read i
  _        -> error "Could not parse input")

solve :: Int -> [Instruction] -> Int
solve knots ins = length $ simulate ins (replicate knots start) (S.singleton start) where
  start :: Point
  start = Point 0 0
  simulate :: [Instruction] -> [Point] -> Set Point -> Set Point
  simulate (i:is) pts s = simulate is npts (s `S.union` S.fromList s') where (npts, s') = executeInstruction i pts
  simulate []     _   s = s
  executeInstruction :: Instruction -> [Point] -> ([Point], [Point])
  executeInstruction i pts = (last iters, map last iters) where
    iters :: [[Point]]
    iters = take n' $ drop 1 $ iterate (move params) pts
    (params, n') = case i of
      Up    n -> ((0,  1), n)
      Down  n -> ((0, -1), n)
      Left  n -> ((-1, 0), n)
      Right n -> (( 1, 0), n)
    move :: (Int, Int) -> [Point] -> [Point]
    move (x, y) (p:ps) = p' : follow (p':ps) where
      p' = p { xCoord = xCoord p + x, yCoord = yCoord p + y }
    move _ [] = []
    follow :: [Point] -> [Point]
    follow (a:b:xs) = b' : follow (b':xs)
      where
        dy = yCoord a - yCoord b
        dx = xCoord a - xCoord b
        sdy = signum dy
        sdx = signum dx
        b' = case (abs dy, abs dx) of
          (2, 0) -> b { yCoord = yCoord b + sdy }
          (0, 2) -> b { xCoord = xCoord b + sdx }
          (2, 1) -> b { xCoord = xCoord a, yCoord = yCoord b + sdy }
          (1, 2) -> b { xCoord = xCoord b + sdx, yCoord = yCoord a }
          (2, 2) -> b { xCoord = xCoord b + sdx, yCoord = yCoord b + sdy }
          (_, _) -> b
    follow _ = []

--  ORIGINAL PART 1 (didn't easily extend to part 2)
-- part1 :: [Instruction] -> Int
-- part1 ins = length $ simulate ins start start (S.singleton start) where
--   simulate :: [Instruction] -> Point -> Point -> Set Point -> Set Point
--   simulate (i:is) hd tl s = simulate is nh (last nts) (s `S.union` S.fromList nts) where (nh, nts) = executeInstruction i hd tl
--   simulate []     _  _  s = s
--   executeInstruction :: Instruction -> Point -> Point -> (Point, [Point])
--   executeInstruction i hd tl =  (fst $ last iters, map snd iters) where
--     iters :: [(Point, Point)]
--     iters = take n' $ drop 1 $ iterate (`move` params) (hd, tl)
--     (params, n') = case i of
--       Up    n -> ((0, 1, True), n)
--       Down  n -> ((0, -1, True), n)
--       Left  n -> ((-1, 0, False), n)
--       Right n -> ((1, 0, False), n)
--   move :: (Point, Point) -> (Int, Int, Bool) -> (Point, Point)
--   move (h, t) (x, y, ud) = (h { xCoord = xCoord h + x, yCoord = yCoord h + y },
--     if ud then if yCoord h - yCoord t == y then t { xCoord = xCoord h, yCoord = yCoord t + y } else t -- If up/down
--           else if xCoord h - xCoord t == x then t { xCoord = xCoord t + x, yCoord = yCoord h } else t) -- If left/right