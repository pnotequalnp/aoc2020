module AoC.Day3 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector ((!), (!?), fromList)

part1 :: Text -> IO Text
part1 = pure . T.pack . show . go 3 1

part2 :: Text -> IO Text
part2 input = pure . T.pack . show . product . fmap ($ input) $ uncurry go <$> slopes
  where
  slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

go :: Int -> Int -> Text -> Int
go i j input = length . filter id $ path 0 0
  where
  grid = fromList $ T.lines input
  width = T.length $ grid ! 0
  path ((`mod` width) -> x) y = case grid !? y of
    Nothing  -> []
    Just row -> (row `T.index` x == '#') : path (x + i) (y + j)
