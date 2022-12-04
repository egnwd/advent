-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Solver  ((:~>)(..))
import           AOC.Common  (countTrue, clearOut)
import           Data.Char   (isDigit)
import           Text.Read   (readMaybe)
import           Data.IntSet (IntSet, isSubsetOf)
import qualified Data.IntSet as IS
import qualified Data.Ix     as Ix

type ElfJobs = IntSet

parseElfPairJobs :: String -> Maybe (ElfJobs, ElfJobs)
parseElfPairJobs s = do
    [a,b,c,d] <- traverse readMaybe . words . clearOut (not . isDigit) $ s
    let l = IS.fromList . Ix.range $ (a,b)
    let r = IS.fromList . Ix.range $ (c,d)
    return (l,r)

day04a :: [(ElfJobs, ElfJobs)] :~> Int
day04a = MkSol
    { sParse = traverse parseElfPairJobs . lines
    , sShow  = show
    , sSolve = Just . countTrue (\(l,r) -> l `isSubsetOf` r || r `isSubsetOf` l)
    }

day04b :: [(ElfJobs, ElfJobs)] :~> Int
day04b = MkSol
    { sParse = traverse parseElfPairJobs . lines
    , sShow  = show
    , sSolve = Just . countTrue (not . IS.null . uncurry IS.intersection)
    }
