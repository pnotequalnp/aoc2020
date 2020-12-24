module AoC.Day7 where

import Data.Foldable (fold)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

type Bag = Text

part1 :: Text -> IO Text
part1 = pure . T.pack . show . M.size . M.filter ("shiny gold" `S.member`) . flatten . parse

part2 :: Text -> IO Text
part2 = pure . T.pack . show . (! "shiny gold") . count . parse

parse :: Text -> Map Bag (Map Bag Int)
parse = M.fromList . fmap (go . T.splitOn " bags contain ") . T.lines
  where
  go [bag, rules] = (bag, rule rules)
  go _ = error "Invalid input"
  rule = M.fromList . catMaybes . fmap (counts . T.splitOn " ") . T.splitOn ", " . T.dropEnd 1
  counts [read . T.unpack -> c, b1, b2, _] = Just (b1 <> " " <> b2, c)
  counts _ = Nothing

flatten :: Map Bag (Map Bag Int) -> Map Bag (Set Bag)
flatten bs = M.map go bs
  where
  go m = M.keysSet m <> fold (M.mapWithKey go' m)
  go' b _ = go $ bs ! b

count :: Map Bag (Map Bag Int) -> Map Bag Int
count bs = M.map (subtract 1 . go) bs
  where
  go = (+1) . sum . M.mapWithKey go'
  go' b x = x * go (bs ! b)
