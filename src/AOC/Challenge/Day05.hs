{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

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
