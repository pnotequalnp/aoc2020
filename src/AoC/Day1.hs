module AoC.Day1 (part1, part2) where

import Control.Monad.ST (runST)
import Data.Foldable (asum)
import qualified Data.HashTable.Class as H
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.Text (Text)
import qualified Data.Text as T

part1 :: Text -> IO Text
part1 input = maybe failure (pure . T.pack . show . \(x,y) -> x*y) result
  where
  input' = read . T.unpack <$> T.lines input
  result = twoSum input' 2020
  failure = fail "No pair of numbers sums to 2020"

part2 :: Text -> IO Text
part2 = undefined

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum xs t = runST $ do
  ht <- H.fromList @HashTable $ pair <$> xs
  asum <$> traverse (find ht) xs
  where
  find ht x = fmap (x,) <$> H.lookup ht x
  pair x = (t - x, x)
