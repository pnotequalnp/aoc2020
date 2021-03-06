module Main (main) where

import Advent as A
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (Clock(..), getTime)
import System.Environment (getArgs, getEnv, lookupEnv)

import qualified AoC.Day1 as Day1
import qualified AoC.Day2 as Day2
import qualified AoC.Day3 as Day3
import qualified AoC.Day4 as Day4
import qualified AoC.Day5 as Day5
import qualified AoC.Day6 as Day6
import qualified AoC.Day7 as Day7
import qualified AoC.Day8 as Day8
import qualified AoC.Day9 as Day9

type Solution = Text -> Text

main :: IO ()
main = do
  [read -> day, read -> PartNumber part] <- getArgs
  key      <- getEnv "AOC_SESSION_KEY"
  year     <- read <$> getEnv "AOC_YEAR"
  cacheDir <- lookupEnv "AOC_CACHE"
  putStrLn $ intercalate "\n"
    [ "Year: " <> show year
    , "Day: " <> show day
    , "Part: " <> show (PartNumber part)
    ]
  let opts = AoCOpts
        { _aSessionKey = key
        , _aYear = year
        , _aCache = cacheDir
        , _aForce = False
        , _aThrottle = 3000000
        }
  case day of
    DayInt day' -> runSolution opts day' $ getSolution day part
    _           -> fail "Day must be within 1-25"

runSolution :: AoCOpts -> Day -> Solution -> IO ()
runSolution opts day solution = do
  res <- runAoC opts $ AoCInput day
  case res of
    Left _      -> fail "Couldn't fetch input"
    Right input -> do
      putStrLn "------------"
      start <- getTime Monotonic
      T.putStrLn $ solution input
      end <- getTime Monotonic
      putStrLn "------------"
      fprint (timeSpecs % "\n") start end

pattern PartNumber :: Bool -> Int
pattern PartNumber x <- (partNumber -> Just x)
  where
  PartNumber = bool 1 2

partNumber :: Int -> Maybe Bool
partNumber = \case
  1 -> Just False
  2 -> Just True
  _ -> Nothing

getSolution :: Integer -> Bool -> Solution
getSolution 1 False = Day1.part1
getSolution 1 True  = Day1.part2
getSolution 2 False = Day2.part1
getSolution 2 True  = Day2.part2
getSolution 3 False = Day3.part1
getSolution 3 True  = Day3.part2
getSolution 4 False = Day4.part1
getSolution 4 True  = Day4.part2
getSolution 5 False = Day5.part1
getSolution 5 True  = Day5.part2
getSolution 6 False = Day6.part1
getSolution 6 True  = Day6.part2
getSolution 7 False = Day7.part1
getSolution 7 True  = Day7.part2
getSolution 8 False = Day8.part1
getSolution 8 True  = Day8.part2
getSolution 9 False = Day9.part1
getSolution 9 True  = Day9.part2
getSolution _ _     = error "Part not completed"
