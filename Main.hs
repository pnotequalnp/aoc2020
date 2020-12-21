module Main (main) where

import Advent as A
import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs, getEnv, lookupEnv)

import qualified AoC.Day1 as Day1

type Solution = Text -> IO Text

main :: IO ()
main = do
  [read -> day, read -> PartNumber part] <- getArgs
  key      <- getEnv "AOC_SESSION_KEY"
  year     <- read <$> getEnv "AOC_YEAR"
  cacheDir <- lookupEnv "AOC_CACHE"
  putStrLn $ unlines
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
    Right input -> solution input >>= T.putStrLn

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
getSolution 1 True = Day1.part2
getSolution _ _ = const $ fail "Part not completed"
