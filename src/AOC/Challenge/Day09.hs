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
import qualified Data.Vector.Unboxed as V
import Control.Monad.State
import Data.Foldable
import Control.Lens
import Control.Lens.TH
import Control.DeepSeq

parseGame = (,) <$> pTok pDecimal <* pTok "players; last marble is worth" <*> pTok pDecimal <* "points"

data Game = MkGame
    { _gMarbles :: V.Vector Int
    , _gCurrent :: Int
    , _gPlayer :: Int
    , _gPlayers :: Int
    } deriving (Show, Generic)

instance NFData Game
$(makeLenses ''Game)

playRound :: Int -> StateT Game Maybe (Map Int Int)
playRound m = do
    player <- use gPlayer
    curr <- use gCurrent
    size <- uses gMarbles V.length
    score <- case m `mod` 23 of
      0 -> do
          let removedIndex = (curr + (7*size) - 7) `mod` size
          (before, after) <- uses gMarbles (V.splitAt removedIndex)
          gMarbles .= before V.++ (V.tail after)
          gCurrent .= removedIndex
          return $ M.singleton player (m+(V.head after))
      _ -> do
          let nextIndex = ((curr + 1) `mod` size) + 1
          (before, after) <- uses gMarbles (V.splitAt nextIndex)
          gMarbles .= before V.++ (m `V.cons` after)
          gCurrent .= nextIndex
          return mempty
    nextPlayer
    return score

playGame (ps, ms) = evalStateT (foldlM (\s -> fmap (M.unionWith (+) s) . playRound) mempty [1..ms]) (MkGame (V.singleton 0) 0 1 ps)

nextPlayer :: (Monad m) => StateT Game m ()
nextPlayer = do
    nPlayers <- use gPlayers
    gPlayer %= (\p -> (p `mod` nPlayers) + 1)

day09a :: _ :~> _
day09a = MkSol
    { sParse = parseMaybeLenient parseGame
    , sShow  = show
    , sSolve = fmap maximum . playGame
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = parseMaybeLenient parseGame
    , sShow  = show
    , sSolve = fmap maximum . playGame . second (*10)
    }
