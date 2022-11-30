{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (revFreq, freqs, countTrue)
import Data.Maybe (isNothing, catMaybes)
import Data.List (find)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Control.Monad (guard)
import qualified Data.IntMap as IM
import qualified Data.Map as M

checksum :: (Ord k, Num a, Num k) => Map k a -> Maybe a
checksum ws = (*) <$> M.lookup 2 ws <*> M.lookup 3 ws

findBox :: [String] -> Maybe String
findBox ws = catMaybes <$> find (\m -> 1 == (countTrue isNothing m)) masks
    where
        masks = mask <$> ws <*> ws
        mask = zipWith (\a b -> a <$ guard (a == b))

day02a :: [String] :~> Int
day02a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = checksum . freqs . concatMap (IM.keys . revFreq)
    }

day02b :: [String] :~> String
day02b = MkSol
    { sParse = Just . lines
    , sShow  = id
    , sSolve = findBox
    }
