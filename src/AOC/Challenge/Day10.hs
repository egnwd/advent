{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Prelude hiding (Point)
import Linear (V2(..), _x, _y)
import qualified Linear as L
import Control.Lens
import Advent.OCR
import qualified Data.Map as M
import qualified Data.Set as S

parseStar :: (Num a) => CharParser (V2 a, V2 a)
parseStar = do
    pos <- V2 <$> (pTok "position=<" *> pDecimal <* pTok ",") <*> (pDecimal <* pTok ">")
    vel <- V2 <$> (pTok "velocity=<" *> pDecimal <* pTok ",") <*> (pDecimal <* pTok ">")
    return (pos, vel)

step (ps, vs) = (zipWith (+) ps vs, vs)

-- Very slow
run t !x = let x' = step x
            in case parseLettersWith (view _x) (view _y) . S.fromList . fst $ x' of
                 Just str -> (t+1, str)
                 Nothing -> run (t+1) x'

day10a :: _ :~> _
day10a = MkSol
    { sParse = fmap unzip . parseLines parseStar
    , sShow  = id
    , sSolve = Just . snd . run 0
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = fmap unzip . parseLines parseStar
    , sShow  = show
    , sSolve = Just . fst . run 0
    }
