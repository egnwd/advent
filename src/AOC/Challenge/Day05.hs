-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common ((!?), listTup, sequenceTuple)

import           Data.IntSet                    (IntSet)
import qualified Data.IntSet                    as IS
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.List                      (sortBy)
import           Data.List.Split                (splitOn)
import           Control.Monad                  (mfilter, (<=<))
import           Text.Read                      (readMaybe)
import           Control.Arrow                  (second)
import           Data.Bifunctor                 (bimap)

isCorrect :: Map Int IntSet -> [Int] -> Bool
isCorrect mp = go IS.empty
    where
        go _ [] = True
        go seen (a:xs') = case M.lookup a mp of
                            Just bf -> IS.disjoint seen bf && go (IS.insert a seen) xs'
                            Nothing -> go (IS.insert a seen) xs'

correct :: Map Int IntSet -> [Int] -> [Int]
correct mp = sortBy comp
    where
        comp a b = case (before a b, before b a) of
                     (Nothing, Nothing) -> EQ
                     (Nothing, Just _) -> GT
                     (Just _, _) -> LT
        before a b = mfilter (IS.member b) (M.lookup a mp)

fetchMiddle :: [a] -> Maybe a
fetchMiddle xs =
    let sz = length xs `div` 2
     in xs !? sz

parseTop :: String -> Maybe (Map Int IntSet)
parseTop = fmap (M.fromListWith IS.union) . traverse (fmap (second IS.singleton) . listTup <=< traverse readMaybe . splitOn "|") . lines
parseBottom :: String -> Maybe [[Int]]
parseBottom = traverse (traverse readMaybe . splitOn ",") . lines

day05a :: _ :~> _
day05a = MkSol
    { sParse = sequenceTuple . bimap parseTop parseBottom <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = uncurry (\mp -> fmap sum . traverse fetchMiddle . filter (isCorrect mp))
    }

day05b :: _ :~> _
day05b = MkSol
    { sParse = sParse day05a
    , sShow  = show
    , sSolve = uncurry (\mp -> fmap sum . traverse (fetchMiddle . correct mp) . filter (not . isCorrect mp))
    }
