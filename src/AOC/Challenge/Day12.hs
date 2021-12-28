-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Solver           ((:~>)(..))
import           AOC.Common           (CharParser, parseLines, lookupFreq)
import           Control.Lens         ((^.), _3)
import           Control.Monad        (guard)
import           Data.Bifunctor       (second)
import           Data.Char            (isUpper)
import           Data.Tuple           (swap)
import           Text.Megaparsec      (many)
import           Text.Megaparsec.Char (char, letterChar)
import qualified Data.Graph as G
import qualified Data.Map as M

type Cave = String
type GraphNeighbours = Cave -> (Bool, Cave, [Cave])
type EnterCavePredicate
    =  M.Map String Int -- ^ Cave -> Number of times visited
    -> Cave             -- ^ Cave name
    -> Bool             -- ^ True when the cave is large
    -> Bool

parser :: CharParser (Cave, Cave)
parser = (,) <$> many letterChar <* char '-' <*> many letterChar

buildGraph :: [(Cave, Cave)] -> GraphNeighbours
buildGraph paths = maybe (False, "end", []) nodeFromVertex . vertexFromKey
    where
        (_, nodeFromVertex, vertexFromKey) = G.graphFromEdges . map createNode . adjs . fixGraphStartEnd $ paths ++ paths'
        createNode (a, b) = (all isUpper a, a, b)
        fixGraphStartEnd = filter (\(a, b) -> a /= "end" && b /= "start")
        adjs = M.toList . M.fromListWith (++) . map (second (:[]))
        paths' = map swap paths

findPaths :: EnterCavePredicate -> GraphNeighbours -> Int
findPaths canEnterCave getNode = sum $ findPaths' M.empty start
    where
        start = getNode "start" ^. _3
        findPaths' _ [] = return 1
        findPaths' seen ns = do
            (large, n, ns') <- map getNode ns
            guard $ canEnterCave seen n large
            let seen' = if large then seen else M.alter (pure . maybe 1 succ) n seen
            findPaths' seen' ns'

part1Predicate, part2Predicate :: EnterCavePredicate
part1Predicate seen a large = large || lookupFreq a seen == 0
part2Predicate seen a large = large || (\s -> s == 0 || s == 1 && all (<2) seen) (lookupFreq a seen)

day12 :: EnterCavePredicate -> GraphNeighbours :~> Int
day12 p = MkSol
    { sParse = fmap buildGraph . parseLines parser
    , sShow  = show
    , sSolve = Just . findPaths p
    }

day12a :: GraphNeighbours :~> Int
day12a = day12 part1Predicate

day12b :: GraphNeighbours :~> Int
day12b = day12 part2Predicate
