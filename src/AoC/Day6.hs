module AoC.Day6 where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

part1 :: Text -> Text
part1 = T.pack . show . sum . fmap count . T.splitOn "\n\n"
  where count = S.size . S.fromList . T.chunksOf 1 . T.filter (/= '\n')

part2 :: Text -> Text
part2 = T.pack . show . sum . fmap count . T.splitOn "\n\n"
  where count = S.size . foldr1 S.intersection . fmap (S.fromList . T.chunksOf 1) . T.lines
