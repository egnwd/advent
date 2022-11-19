{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Prelude
import Control.Monad.Combinators
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Lens
import Data.Function (on)

data Instruction = Instruction
    { iTime :: Time
    , iCommand :: Command
    } deriving (Show, Eq, Ord)
type Time = (Int, Int, Int, Int, Int)
data Command = Guard Int | Wake | Sleep deriving (Show, Eq, Ord)

year   (y, _, _, _, _) = y
month  (_, m, _, _, _) = m
day    (_, _, d, _, _) = d
hour   (_, _, _, h, _) = h
minute (_, _, _, _, m) = m

parseInstruction :: CharParser Instruction
parseInstruction = Instruction <$> parseTime <*> parseCommand
    where
        parseTime = (,,,,) <$> ("[" *> pDecimal <* "-") <*> (pDecimal <* "-") <*> (pTok pDecimal) <*> (pDecimal <* ":") <*> (pDecimal <* pTok "]")
        parseCommand = choice
            [ Guard <$> ("Guard #" *> pTok pDecimal <* "begins shift")
            , Sleep <$ "falls asleep"
            , Wake <$ "wakes up"
            ]

type SleepLog = M.Map Int (IM.IntMap Int)

snoozers :: [Instruction] -> SleepLog
snoozers = snd . foldl' go (Nothing, M.empty)
    where
        go (g, l) (Instruction t c) = case c of
            Guard n -> (Just n, M.insertWith (flip const) n mempty l)
            Wake -> (g, maybe l (\n -> M.adjust (wakeup t) n l) g)
            Sleep -> (g, maybe l (\n -> M.adjust (fallAsleep t) n l) g)
        wakeup, fallAsleep :: Time -> IM.IntMap Int -> IM.IntMap Int
        wakeup t zzz = foldl' (flip $ IM.alter (fmap pred)) zzz [minute t..59]
        fallAsleep t zzz = foldl' (flip $ IM.alter (pure . maybe 1 succ)) zzz [minute t..59]

minutesMostAsleep :: Ord a => IM.IntMap a -> Maybe (Int, a)
minutesMostAsleep m = do
  maxValue <- maximumOf traverse m
  listToMaybe . IM.assocs . IM.filter (== maxValue) $ m

findSleepiestGuard = fmap (\(g, (m, _)) -> g * m) . sequence . second minutesMostAsleep . maximumBy (compare `on` (sum . snd)) . M.assocs

mostFrequentlyAsleep = fmap (\(g, (m, _)) -> g * m) . sequence . maximumBy (compare `on` (fmap snd . snd)) . M.assocs . M.map minutesMostAsleep

day04a :: _ :~> _
day04a = MkSol
    { sParse = fmap (sortOn iTime) . parseLines parseInstruction
    , sShow  = show
    , sSolve = findSleepiestGuard . snoozers
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = fmap (sortOn (\(Instruction t _) -> t)) . parseLines parseInstruction
    , sShow  = show
    , sSolve = mostFrequentlyAsleep . snoozers
    }
