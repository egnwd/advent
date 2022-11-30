{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Prelude
import qualified Data.Sequence as S
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import Data.List.PointedList.Circular (PointedList)

type Recipes = PointedList Int
type Elves = (Int, Int)

newRecipes = S.unfoldl (\x -> x `divMod` 10 <$ guard (x > 0))

starterRecipes :: Recipes
starterRecipes = fromJust $ PLC.fromList [3,7]

round :: (Elves, Recipes) -> Maybe (Elves, Recipes)
round ((e1,e2), rs) = undefined
--    score1 <- preview PL.focus =<< PL.moveTo e1 rs
--    score2 <- preview PL.focus =<< PL.moveTo e2 rs
--    rsEnd <- PL.moveTo ((PL.length rs) - 1) rs
--    let rs' = foldl (\r n -> PL.insert n r) (newRecipes (score1 + score2))
--    return ((e1',e2'), rs')

day14a :: Int :~> _
day14a = MkSol
    { sParse = readMaybe
    , sShow  = show
    , sSolve = Just
    }

day14b :: _ :~> _
day14b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
