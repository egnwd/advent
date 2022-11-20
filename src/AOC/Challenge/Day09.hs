{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Prelude
import qualified Data.Map as M
import qualified Data.List.PointedList.Circular as PL
import Data.Foldable
import Control.Lens

parseGame = (,) <$> pTok pDecimal <* pTok "players; last marble is worth" <*> pTok pDecimal <* "points"

playRound m marbles
  | m `mod` 23 == 0 = let marbles' = PL.moveN (-7) marbles
                          current = marbles' ^. PL.focus
                       in (m+current, fromJust (PL.delete marbles'))
  | otherwise = (0, PL.insertLeft m . PL.moveN 2 $ marbles)

playGame :: (Int, Int) -> _
playGame (ps, ms) = fst $ foldl' go (M.empty, PL.singleton 0) (zip (cycle [1..ps]) [1..ms])
    where
        go (!scores, !marbles) (!p, !m) = (M.insertWith (+) p s scores, marbles')
            where
                (!s, !marbles') = playRound m marbles

day09a :: _ :~> _
day09a = MkSol
    { sParse = parseMaybeLenient parseGame
    , sShow  = show
    , sSolve = Just . maximum . playGame
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = parseMaybeLenient parseGame
    , sShow  = show
    , sSolve = Just . maximum . playGame . second (*100)
    }
