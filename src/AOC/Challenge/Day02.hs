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

import AOC.Common ( pDecimal, parseLines )
import AOC.Solver ( (:~>)(..) )
import Data.Monoid ( Sum(getSum) )
import Text.Megaparsec.Char ( char )
import Control.Monad.Combinators ( sepBy )
import Control.Lens.Fold ( productOf )
import Control.Lens.Each ( Each(each) )
import Control.Monad ( join )

listTup3 :: [a] -> Maybe (a,a,a)
listTup3 (x:y:z:_) = Just (x,y,z)
listTup3 _         = Nothing

parta :: (Num a, Ord a) => (a, a, a) -> a
parta (x, y, z) = let sides = [x*y, y*z, x*z] in 2 * sum sides + minimum sides

partb :: (Num a, Ord a) => (a, a, a) -> a
partb p@(x, y, z) = let edges = [2*(x+y), 2*(y+z), 2*(z+x)] in minimum edges + productOf each p

parcelParser :: String -> Maybe [(Sum Integer, Sum Integer, Sum Integer)]
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
