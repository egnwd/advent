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
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

data Rock = Cube | Rolly deriving (Eq, Ord, Show)

projectRock Cube = '#'
projectRock Rolly = 'O'

parseRock '#' = Just Cube
parseRock 'O' = Just Rolly
parseRock _ = Nothing

cycleRocks = roll East
           . roll South
           . roll West
           . roll North

roll d mp0 = M.foldlWithKey go (M.filter (==Cube) mp0) mp0
    where
        Just bb = boundingBox <$> (NES.nonEmptySet . M.keysSet $ mp0)
        step (k,mp) = case M.lookup (k+dirVec d) mp of
                        Just Cube -> Left k
                        Just Rolly -> Left k
                        Nothing -> if not (inBoundingBox bb (k+dirVec d))
                                      then Left k
                                      else Right (k+dirVec d,mp)
        stepBack (k,mp) = case M.lookup k mp of
                            Just Rolly -> Right (k-dirVec d, mp)
                            _ -> Left k
        go mp k Cube = mp
        go mp k Rolly = let mnk = loopEither stepBack . (,mp) . loopEither step $ (k,mp)
                         in M.insert mnk Rolly mp

calculateLoad :: Map Point Rock -> Maybe Int
calculateLoad mp = do
    mx <- (`getEdge` South) . boundingBox <$> (NES.nonEmptySet . M.keysSet $ mp)
    return $ M.foldlWithKey (\a (L.V2 _ y) b -> case b of { Cube -> a; Rolly -> a + (1 + mx - y); }) 0 mp

firstRepeatedCount :: Ord b => [b] -> Maybe (Int, Int, b)
firstRepeatedCount = go M.empty 0
  where
      go seen t (x:xs) = case M.lookup x seen of
                           Just t' -> Just (t', t-t', x)
                           Nothing -> go (M.insert x t seen) (t+1) xs
      go _ _ []     = Nothing

toEnd (n, d, x) = let i = (1000000000-n) `mod` d
                   in head . drop i . iterate cycleRocks $ x

day14a :: _ :~> _
day14a = MkSol
    { sParse = Just . parseAsciiMap parseRock
    , sShow  = show
    , sSolve = calculateLoad . roll North
    }

day14b :: _ :~> _
day14b = MkSol
    { sParse = Just . parseAsciiMap parseRock
    , sShow  = show
    , sSolve = calculateLoad . toEnd <=< firstRepeatedCount . iterate cycleRocks
    }
