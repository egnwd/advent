{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Prelude
import AOC.Common.Point
import Linear.V3
import Control.Lens.TH
import Control.Lens

data Moon a = Moon
    { _mLoc :: a
    , _mVelocity :: a
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
$(( makeLenses 'Moon ))

parseMoon :: CharParser (Moon Point3D)
parseMoon = do
 loc <- V3 <$> ("<x=" *> pDecimal) <*> (", y=" *> pDecimal) <*> (", z=" *> pDecimal) <* ">"
 return $ Moon loc 0

step :: (Applicative f, Num b, Num (f b), Ord b) => [Moon (f b)] -> [Moon (f b)]
step ms
    = map (\m -> m & mLoc +~ (m ^. mVelocity)) -- ^ move the moon
    . map (\m -> m & mVelocity +~ acc m)       -- ^ apply gravity
    $ ms
        where
            acc m = getSum . foldMap (gravity m) $ ms
            gravity (Moon m1 _) (Moon m2 _) = Sum . signum $ subtract m1 m2

totalEnergy :: [Moon (V3 Int)] -> Int
totalEnergy = getSum . foldMap (Sum . product . fmap (sum . abs))

day12a :: _ :~> _
day12a = MkSol
    { sParse = parseLines parseMoon
    , sShow  = show
    , sSolve = fmap totalEnergy . (!? (dyno_ "steps" 1000)) . iterate step
    }

day12b :: _ :~> _
day12b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
