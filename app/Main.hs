module Main where

import Options.Applicative
import qualified Days

data Options = Options
  { day :: Int
  , numCores :: Int
  }

options :: Parser Options
options = Options
  <$> option auto
      ( long "day"
      <> short 'd'
      <> metavar "DAY"
      <> help "Which day to run (1-25)" )
  <*> option auto
      ( long "cores"
      <> short 'c'
      <> metavar "CORES"
      <> value 1
      <> help "Number of cores to use" )

main :: IO ()
main = runDay =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "Run Advent of Code solutions"
      <> header "advent-of-code - solutions runner" )

runDay :: Options -> IO ()
runDay (Options d cores) = 
  case d of
    2  -> Days.day02
    3  -> Days.day03
    4  -> Days.day04
    5  -> Days.day05
    6  -> Days.day06
    7  -> Days.day07
    8  -> Days.day08
    9  -> Days.day09
    10 -> Days.day10
    11 -> Days.day11
    12 -> Days.day12
    13 -> Days.day13
    14 -> Days.day14
    15 -> Days.day15
    16 -> Days.day16
    _  -> putStrLn $ "Day " ++ show d ++ " not implemented yet"
