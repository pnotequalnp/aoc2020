module AoC.Day4 where

import Control.Monad (guard)
import Data.Char (isDigit, isHexDigit)
import Data.List.Split (splitOn)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

type Passport = Map Text Text

part1 :: Text -> Text
part1 input = T.pack . show . length . filter validFields . parse $ input

part2 :: Text -> Text
part2 input = T.pack . show . length . filter valid . parse $ input

parse :: Text -> [Passport]
parse t =
  M.fromList . fmap toPair . T.splitOn " " . T.intercalate " " <$> (splitOn [""] . T.lines) t
  where
  toPair x = let [k, v] = T.splitOn ":" x in (k,v)

validFields :: Passport -> Bool
validFields p = required `S.isSubsetOf` M.keysSet p

valid :: Passport -> Bool
valid p = maybe False (const True) do
  p !? "byr" >>= fourNumbers 1920 2002
  p !? "iyr" >>= fourNumbers 2010 2020
  p !? "eyr" >>= fourNumbers 2020 2030
  T.span isDigit <$> p !? "hgt" >>= uncurry hgt
  p !? "hcl" >>= T.uncons >>= uncurry hcl
  p !? "ecl" >>= guard . (`elem` [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ])
  p !? "pid" >>= guard . ((&&) <$> numeric <*> (==9) . T.length)
  where
  between i j x = i <= x && x <= j
  fourNumbers i j x = do
    x' <- readMaybe @Int . T.unpack $ x
    guard $ between i j x'
  numeric = T.all isDigit
  hcl s r = guard $ s == '#' && T.all isHexDigit r
  hgt (read @Int . T.unpack -> h) u
    | u == "cm" = guard $ between 150 193 h
    | u == "in" = guard $ between 59 76 h
    | otherwise = Nothing

required :: S.Set Text
required = S.fromList [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]
