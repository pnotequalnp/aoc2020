module AoC.Day5 where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float (double2Int)

type Seat = (Int, Int)

part1 :: Text -> IO Text
part1 = pure . T.pack . show . maximum . fmap (seatId . decode) . T.lines

part2 :: Text -> IO Text
part2 input = pure . T.pack . show $ expected - actual
  where
  ids = seatId . decode <$> T.lines input
  n = length ids + 1
  low = minimum ids
  high = maximum ids
  expected = double2Int $ (fromIntegral n / 2) * (fromIntegral low + fromIntegral high)
  actual = fromIntegral $ sum ids

seatId :: Seat -> Int
seatId (r, c) = 8 * r + c

decode :: Text -> Seat
decode t = (r, c)
  where (readBin rDig -> r, readBin cDig -> c) = T.splitAt 7 t

readBin :: (Char -> Int) -> Text -> Int
readBin dig = T.foldl' (\a x -> a * 2 + dig x) 0

rDig :: Char -> Int
rDig = \case 'F' -> 0; _ -> 1

cDig :: Char -> Int
cDig = \case 'L' -> 0; _ -> 1
