module AoC.Day2 where

import Data.Text (Text)
import qualified Data.Text as T

part1 :: Text -> IO Text
part1 = pure . T.pack . show . length . filter valid . T.lines
  where
  valid t = x >= l && x <= u
    where
    (l, u, c, r) = parse t
    x = T.length . T.filter (==c) $ r

part2 :: Text -> IO Text
part2 = pure . T.pack . show . length . filter valid . T.lines
  where
  valid t = (r `T.index` (l-1) == c) /= (r `T.index` (u-1) == c)
    where
    (l, u, c, r) = parse t

parse :: Text -> (Int, Int, Char, Text)
parse t = (l, u, c, r)
  where
  (read . T.unpack -> l, T.drop 1 -> t') = T.breakOn "-" t
  (read . T.unpack -> u, T.drop 1 -> t'') = T.breakOn " " t'
  (T.head -> c, T.drop 2 -> r) = T.splitAt 1 t''
