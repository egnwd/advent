{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude
import Linear.V2
import qualified Data.Map as M
import qualified Data.Set as S

parseClaim = do
    i <- "#" *> pTok pDecimal <* pTok "@"
    coord <- V2 <$> pDecimal <* "," <*> pDecimal <* pTok ":"
    size <- V2 <$> pDecimal <* "x" <*> pDecimal
    return $ Claim i coord size

data Claim = Claim
    { cId :: Int
    , cCoord :: Point
    , cSize :: V2 Int
    }
    deriving (Eq, Show)

corners (Claim {..}) = V2 cCoord (cCoord + cSize - 1)

allSquares c = sequence (V2 [xMn..xMx] [yMn..yMx])
    where
        V2 (V2 xMn yMn) (V2 xMx yMx) = corners c

findGoodClaim cs = find good cs
    where
        good c = let section = S.fromList (allSquares c)
                  in section `S.isSubsetOf` fabric
        fabric = M.keysSet . M.filter (== 1) . freqs . concatMap allSquares $ cs

day03a :: _ :~> _
day03a = MkSol
    { sParse = parseLines parseClaim
    , sShow  = show
    , sSolve = Just . M.size . M.filter (> 1) . freqs . concatMap allSquares
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = parseLines parseClaim
    , sShow  = show
    , sSolve = fmap cId . findGoodClaim
    }
