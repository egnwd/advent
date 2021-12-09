{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import Linear
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

parse = parseAsciiMap (readMaybe @Int . (:[]))

solve :: M.Map Point Int -> _
solve nss = M.foldr (\a b -> b + (1+a)) 0 (M.filterWithKey go nss)
    where
        go k h = all (h<) $ map (nss M.!) (neighbours nss k)

solveb :: M.Map Point Int -> _
solveb nss = getProduct . AOC.Prelude.fold $ basins
    where
        basins = take 3 . sortOn (*(-1)) . map snd .  M.toList $ M.mapWithKey (\l _ -> Product . getSum . AOC.Prelude.fold . apo (buildBasin nss) $ (S.empty, S.singleton l)) lowPoints
        lowPoints = (M.filterWithKey go nss)
        go k h = all (h<) $ map (nss M.!) (neighbours nss k)

neighbours nss k = [k' | k' <- (k +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ], inBoundingBox (V2 (fst . M.findMin $ nss) (fst . M.findMax $ nss)) k']

buildBasin :: M.Map Point Int -> (S.Set Point, S.Set Point) -> ListF (Sum Int) (Either [Sum Int] (S.Set Point, S.Set Point))
buildBasin nss (seen, next) = Cons (Sum (S.size next)) go
    where
        next' = flip S.difference seen . M.keysSet . M.filter (/=9) . M.restrictKeys nss . S.fromList . concat . S.toList . S.map (neighbours nss) $ next
        seen' = S.union seen next
        go = if null next'
                then Left []
                else Right (seen', next')

day09a :: _ :~> _
day09a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solveb
    }
