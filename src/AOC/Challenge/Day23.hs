-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Solver ((:~>)(..), dyno_)
import           AOC.Common (Point, Dir(..), boundingBox, parseAsciiSet, statefulIndexedFixedPoint, freqs, lookupFreq, dirVec, allNeighboursSet)
import           Linear
import           Data.Ix
import           Data.Monoid
import           Data.Bits
import           Data.Word
import           Data.Maybe (fromMaybe)
import           Control.Monad (guard)
import           Data.Set (Set)
import           Data.Map (Map)

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES

type Mask = Word8

northEdge, eastEdge, southEdge, westEdge :: Mask
northEdge = 148 -- 10010100
eastEdge  = 7   -- 00000111
southEdge = 41  -- 00101001
westEdge  = 224 -- 11100000

dirToEdge :: Dir -> Mask
dirToEdge North = northEdge
dirToEdge East = eastEdge
dirToEdge South = southEdge
dirToEdge West = westEdge

step :: ([Dir], Set Point) -> ([Dir], Set Point)
step (ds, elves0) = (ds', elves')
    where
        ds' = take 4 . drop 1 . cycle $ ds
        elves' = move . propose $ elves0

        propose :: Set Point -> Map Point Point
        propose = M.fromSet chooseLocation

        chooseLocation :: Point -> Point
        chooseLocation e
          | ns == 0 = e
          | otherwise = fromMaybe e . getAlt . foldMap (Alt . newElfLocation e ns) $ ds
          where ns = neighbourMask e

        neighbourMask :: Point -> Mask
        neighbourMask e = S.foldl' (\b p -> (hasElf p) (b `shiftL` 1)) 0 $ allNeighboursSet e

        hasElf :: Point -> Mask -> Mask
        hasElf p
          | p `S.member` elves0 = flip setBit 0
          | otherwise = flip const 0

        newElfLocation :: Point -> Mask -> Dir -> Maybe Point
        newElfLocation e ns d = (e + dirVec d) <$ guard (ns .&. (dirToEdge d) == 0)

        move :: Map Point Point -> Set Point
        move props = fromElems . M.mapWithKey (\k a -> if lookupFreq a fs > 1 then k else a) $ props
            where
                fromElems = S.fromList . M.elems
                fs = freqs props

emptyGround :: Set Point -> Maybe Int
emptyGround elves = do
    (V2 mn mx) <- fmap boundingBox . NES.nonEmptySet $ elves
    return $ rangeSize (mn, mx) - S.size elves

startingChecks :: [Dir]
startingChecks = [North, South, West, East]

day23a :: Set Point :~> Int
day23a = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = emptyGround . snd . head . drop (dyno_ "steps" 10) . iterate step . (startingChecks,)
    }

day23b :: Set Point :~> Int
day23b = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . fst . statefulIndexedFixedPoint step . (startingChecks,)
    }
