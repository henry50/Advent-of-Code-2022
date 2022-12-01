module Main (main) where

import Options.Applicative
    ( optional,
      argument,
      eitherReader,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strOption,
      switch,
      execParser,
      helper,
      Parser,
      ReadM )
import Control.Monad (when)
import Text.Read (readMaybe)
import Control.Exception (try)
import Control.Exception.Base (IOException)

import Day00 (runDay00)
import Day01 (runDay01)
import Day02 (runDay02)
import Day03 (runDay03)
import Day04 (runDay04)
import Day05 (runDay05)
import Day06 (runDay06)
import Day07 (runDay07)
import Day08 (runDay08)
import Day09 (runDay09)
import Day10 (runDay10)
import Day11 (runDay11)
import Day12 (runDay12)
import Day13 (runDay13)
import Day14 (runDay14)
import Day15 (runDay15)
import Day16 (runDay16)
import Day17 (runDay17)
import Day18 (runDay18)
import Day19 (runDay19)
import Day20 (runDay20)
import Day21 (runDay21)
import Day22 (runDay22)
import Day23 (runDay23)
import Day24 (runDay24)
import Day25 (runDay25)

import Part ( Part(..) )

days :: [Part -> [String] -> [String]]
days = [runDay00, runDay01, runDay02, runDay03, runDay04, runDay05, runDay06, runDay07, runDay08, runDay09, runDay10, runDay11, runDay12, runDay13, runDay14, runDay15, runDay16, runDay17, runDay18, runDay19, runDay20, runDay21, runDay22, runDay23, runDay24, runDay25]

data Options = Options{
    input :: Maybe FilePath,
    makeFiles :: Bool,
    day :: Int,
    part :: Part
}

parseDayNumber :: ReadM Int
parseDayNumber = eitherReader $ \s -> case readMaybe s :: Maybe Int of
    Nothing -> Left "Could not parse day to integer."
    Just i -> if (i >= 0) && (i <= 25) then Right i else Left "Day not in range."

parsePartNumber :: ReadM Part
parsePartNumber = eitherReader $ \s -> case readMaybe s :: Maybe Int of
    Nothing -> Left "Could not parse part number to integer."
    Just i -> case i of
        1 -> Right One
        2 -> Right Two
        _ -> Left "Invalid part number."

optionParser :: Parser Options
optionParser = Options <$>
    optional (strOption (
        long "input" <>
        short 'i' <>
        metavar "FILE" <>
        help "Specify an input file to use, defaults to data/daynn.txt"
    )) <*>
    switch (
        long "makefiles" <>
        help "Create Daynn.hs files on /src for all days"
    ) <*>
    argument parseDayNumber (metavar "DAY") <*>
    argument parsePartNumber (metavar "PART")

main :: IO ()
main = do
    options <- execParser $ info (helper <*> optionParser) (progDesc "AOC 2022 CLI")
    when (makeFiles options) $ do
        putStrLn "Creating files..."
        makeDays
        putStrLn "Complete."
        return ()
    let dayRunner = days !! day options
    let partNumber = part options
    let paddedDay =  padDayNumber $ day options
    let inputPath = case input options of
            Nothing -> "data/day" ++ paddedDay ++ ".txt"
            Just s -> s
    inputString <- try (readFile inputPath) :: IO (Either IOException String)
    case inputString of
      Left _ -> do
        putStrLn ("Could not read file with path \"" ++ inputPath ++ "\", exiting")
        return ()
      Right s -> mapM_ putStrLn $ dayRunner partNumber (lines s)

padDayNumber :: Int -> String
padDayNumber n = if n < 10 then "0" ++ show n else show n

makeDays :: IO ()
makeDays = do
    mapM_ mkFile [1..25] where
        mkFile :: Int -> IO ()
        mkFile n = writeFile ("src/Day" ++ d ++ ".hs") ("module Day" ++ d ++ " (runDay" ++ d ++ ") where\n\nimport Part ( Part(..) )\n\n\nrunDay" ++ d ++ " :: Part -> [String] -> [String]\nrunDay" ++ d ++ " p = case p of\n  One -> error \"Not implemented\"\n  Two -> error \"Not implemented\"\n") where
            d = padDayNumber n

