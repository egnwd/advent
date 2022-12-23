-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Prelude
import           Linear
import           Data.Ix
import           Data.Monoid

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES

step :: ([Dir], NES.NESet Point) -> ([Dir], NES.NESet Point)
step (ds, elves0) = (ds', elves')
    where
        ds' = take 4 . drop 1 . cycle $ ds
        es0 = NES.toSet elves0
        elves' = move . propose $ elves0

        propose :: NES.NESet Point -> Map Point Point
        propose = M.fromSet chooseLocation . NES.toSet

        chooseLocation :: Point -> Point
        chooseLocation e
          | S.disjoint es0 (allNeighboursSet e) = e
          | otherwise = fromMaybe e . getAlt . foldMap (Alt . elvesInDirection e) $ ds

        elvesInDirection :: Point -> Dir -> Maybe Point
        elvesInDirection e d
          | S.disjoint es0 ((dirs d) e) = Just $ e + dirVec d
          | otherwise = Nothing

        dirs :: Dir -> Point -> Set Point
        dirs North e = S.map (+e) northEdge
        dirs East e  = S.map (+e) eastEdge
        dirs South e = S.map (+e) southEdge
        dirs West e  = S.map (+e) westEdge

        move :: Map Point Point -> NES.NESet Point
        move props = toNonEmptySet . M.mapWithKey (\k a -> if lookupFreq a fs > 1 then k else a) $ props
            where
                toNonEmptySet = NES.unsafeFromSet . S.fromList . M.elems
                fs = freqs props

emptyGround :: NES.NESet Point -> Int
emptyGround elves = totalArea - NES.size elves
    where
        totalArea = rangeSize (mn, mx)
        (V2 mn mx) = boundingBox elves

startingChecks :: [Dir]
startingChecks = [North, South, West, East]

day23a :: NES.NESet Point :~> Int
day23a = MkSol
    { sParse = NES.nonEmptySet . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . emptyGround . snd . head . drop (dyno_ "steps" 10) . iterate step . (startingChecks,)
    }

day23b :: NES.NESet Point :~> Int
day23b = MkSol
    { sParse = NES.nonEmptySet . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . fst . statefulIndexedFixedPoint step . (startingChecks,)
    }
