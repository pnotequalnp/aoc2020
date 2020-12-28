{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module AoC.Day8 where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

type Program = Vector Instruction
type Machine = ExceptT Exit (State S)

data Instruction = Nop Int | Acc Int | Jmp Int deriving Show
data Exit = Success Int | Loop Int | Error deriving Show
data S = S { _acc :: Int , _pc  :: Int , _ins :: Set Int }
makeLenses ''S

part1 :: Text -> Text
part1 = T.pack . show . go . runProgram . parseInstructions
  where go = \case Loop x -> x; _ -> error "Invalid input"

part2 :: Text -> Text
part2 = T.pack . show . head . mapMaybe go . fmap runProgram . corrected . parseInstructions
  where go = \case Success x -> Just x; _ -> Nothing

corrected :: Program -> [Program]
corrected p = V.toList . V.map (swapInstruction p) $
  V.findIndices (\case Acc _ -> True; Jmp _ -> True; _ -> False) p

parseInstructions :: Text -> Program
parseInstructions = V.fromList . fmap (go . T.splitOn " ") . T.lines
  where
  go [com, x] = (case com of "nop" -> Nop; "acc" -> Acc; "jmp" -> Jmp; _ -> error "Invalid input")
    . read . T.unpack . T.filter (/= '+') $ x
  go _ = error "Invalid input"

swapInstruction :: Program -> Int -> Program
swapInstruction is n = case is ! n of
  Nop x -> is // [(n, Jmp x)]
  Jmp x -> is // [(n, Nop x)]
  _     -> is

runProgram :: Program -> Exit
runProgram = flip evalState (S 0 0 S.empty) . run . step

run :: Monad m => ExceptT e m a -> m e
run x = go
  where go = runExceptT x >>= \case
          Left e  -> pure e
          Right _ -> go

step :: Program -> Machine ()
step p = do
  S { _acc, _ins, _pc } <- get
  let l = V.length p
  when (_pc == l) $ throwError $ Success _acc
  when (_pc > l) $ throwError Error
  when (_pc `S.member` _ins) $ throwError $ Loop _acc
  ins %= S.insert _pc
  case p ! _pc of
    Nop _ -> pc += 1
    Acc x -> (acc += x) *> (pc += 1)
    Jmp x -> pc += x
