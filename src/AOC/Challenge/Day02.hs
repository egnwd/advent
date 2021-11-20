{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import AOC.Common
import AOC.Solver
import Data.Monoid
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Control.Lens.Fold
import Control.Lens.Each
import Control.Monad

listTup3 :: [a] -> Maybe (a,a,a)
listTup3 (x:y:z:_) = Just (x,y,z)
listTup3 _         = Nothing

parta (x, y, z) = let sides = [x*y, y*z, x*z] in 2 * sum sides + minimum sides

partb p@(x, y, z) = let edges = [2*(x+y), 2*(y+z), 2*(z+x)] in minimum edges + productOf each p

parcelParser = mapM join . sequence <$> parseLines (listTup3 <$> (pDecimal `sepBy` char 'x'))

day02a :: _ :~> _
day02a = MkSol
  { sParse = parcelParser
  , sShow  = show
  , sSolve = Just . getSum . foldMap parta
  }

day02b :: _ :~> _
day02b = MkSol
  { sParse = parcelParser
  , sShow  = show
  , sSolve = Just . getSum . foldMap partb
  }
