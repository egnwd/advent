{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Prelude hiding (Space, allShortestPaths)

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import           Data.OrdPSQ                    (OrdPSQ)
import qualified Data.OrdPSQ                    as Q
import qualified Data.Sequence.NonEmpty         as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Control.Lens
import Control.Lens.TH

data ReindeerMap = Start | Space | End deriving (Show, Eq, Ord)

parse 'S' = Just Start
parse 'E' = Just End
parse '.' = Just Space
parse _ = Nothing

toPoints mp = do
    let points = M.fromListWith (<>) . map (\(k, x) -> (x, S.singleton k)) . M.toList $ mp
    [s] <- S.toList <$> M.lookup Start points
    [e] <- S.toList <$> M.lookup End points
    return (s, e, fold points)

newtype AStarAllState a b c = AStarAllState { _nodeQueue :: OrdPSQ a (Dist c) (Seq.NESeq (Set b)) }

$(makeLenses ''AStarAllState)

initialAStarAllState :: Num c => a -> b -> AStarAllState a b c
initialAStarAllState s b = AStarAllState (Q.singleton s 0 (Seq.singleton (S.singleton b)))

-- | TODO: Include Heur to improve perf
allShortestPaths
  :: forall a b c. (Ord a, Ord b, Ord c, Num c, Show c, Show a, Show b)
  => (a -> b) -- ^ prune
  -> (a -> c) -- ^ heuristic
  -> (a -> Map a c) -- ^ neighbourhood
  -> (a -> Bool)    -- ^ termination condition
  -> a              -- ^ start
  -> Maybe (Dist c, Set b) -- ^ perhaps the cost with the path
allShortestPaths toSpace heur next end start = second fold <$> go (initialAStarAllState start (toSpace start))
  where
    go :: AStarAllState a b c -> Maybe _
    go ds@(AStarAllState q0) =
      case Q.minView q0 of
        Nothing -> Nothing
        Just (n, c, p Seq.:<|| ps, q)
          | end n -> Just (c, p : goAgain c (AStarAllState $ Q.fromList . fst . Q.atMostView c $ q'))
          | otherwise -> let ds' = ds & nodeQueue .~ q'
                             !ns = M.map Dist (next n)
                          in go $ M.foldlWithKey' (updateNeighbour p c) ds' ns
          where
              q' = case Seq.nonEmptySeq ps of
                     Nothing -> q
                     Just xs -> Q.insert n c xs q

    goAgain :: Dist c -> AStarAllState a b c -> [Set b]
    goAgain minDist ds =
      case Q.minView (ds ^. nodeQueue) of
        Nothing -> []
        Just (n, c, p Seq.:<|| ps, q)
          | end n -> p : goAgain minDist (AStarAllState $ Q.fromList . fst . Q.atMostView minDist $ q')
          | otherwise -> let ds' = ds & nodeQueue .~ q'
                             !ns = M.map Dist (next n)
                          in goAgain minDist . (nodeQueue %~ Q.fromList . fst . Q.atMostView minDist) $ M.foldlWithKey' (updateNeighbour p c) ds' ns
          where
              q' = case Seq.nonEmptySeq ps of
                     Nothing -> q
                     Just xs -> Q.insert n c xs q

    updateNeighbour :: Set b -> Dist c -> AStarAllState a b c -> a -> Dist c -> AStarAllState a b c
    updateNeighbour pth c ds n w =
      let cost' = w + c
      in if toSpace n `S.member` pth
            then ds
            else case Q.lookup n (ds ^. nodeQueue) of
                   Nothing -> ds & nodeQueue %~ Q.insert n cost' (Seq.singleton (S.insert (toSpace n) pth))
                   Just (cost, pths)
                     | cost' == cost -> ds & nodeQueue %~ Q.insert n cost' (pths Seq.|> S.insert (toSpace n) pth)
                     | cost' < cost -> ds & nodeQueue %~ Q.insert n cost' (Seq.singleton (S.insert (toSpace n) pth))
                     | otherwise -> ds

solve :: (Point, Dir) -> Point -> Set Point -> Maybe (Dist Int, _)
solve start end mp = allShortestPaths (second (`elem` [North, South])) heur next ((== end) . fst) start
    where
        next (p,d) = M.fromList [((p',d'),c) | ((p',d'), c) <- [((p, d <> East), 1000), ((p, d <> West), 1000), ((p + dirVec d, d), 1)], p' `S.member` mp]
        heur = manhattan end . fst

day16a :: _ :~> _
day16a = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, mp) -> fromDist . fst =<< solve (s, East) e mp
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, mp) -> S.size . S.map fst . snd <$> solve (s, East) e mp
    }
