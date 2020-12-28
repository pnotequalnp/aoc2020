module AoC.Day9 where

import Control.Lens
import Control.Monad.ST (runST)
import Data.Foldable (asum, toList)
import qualified Data.HashTable.Class as H
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.List (find, inits, tails)
import Data.Maybe (isNothing)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

part1 :: Text -> Text
part1 = T.pack . show . nonSum . fmap (read @Int . T.unpack) . T.lines

part2 :: Text -> Text
part2 inp =
  T.pack . show . ((+) <$> maximum <*> minimum) . head . filter ((==x) . sum) . sublists $ xs
  where
  xs = fmap (read @Int . T.unpack) . T.lines $ inp
  x = nonSum xs

nonSum :: [Int] -> Int
nonSum = go' . find (isNothing . uncurry twoSum) . uncurry go . (_1 %~ Seq.fromList) . splitAt 25
  where
  go _ [] = []
  go zs@(_:<|ys) (x:xs) = (x, zs) : go (ys :|> x) xs
  go _ _ = error "Invalid input"
  go' (Just (x,_)) = x
  go' _ = error "Invalid input"

twoSum :: Int -> Seq Int -> Maybe (Int, Int)
twoSum t xs = runST $ do
  ht <- H.fromList @HashTable . toList $ pair <$> xs
  asum <$> traverse (find' ht) xs
  where
  find' ht x = fmap (x,) <$> H.lookup ht x
  pair x = (t - x, x)

sublists :: [a] -> [[a]]
sublists = filter go . concatMap tails . inits
  where go = \case [] -> False; [_] -> False; [_,_] -> False; _ -> True
