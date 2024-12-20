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

import           AOC.Prelude hiding (Space)

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import           Data.OrdPSQ                    (OrdPSQ)
import qualified Data.OrdPSQ                    as Q
import qualified Data.Sequence                  as Seq
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

data AStarState a c = AS
  { _asCameFrom :: !(Map a (Maybe a))
  , _asOpenSet :: !(OrdPSQ a c (c, Maybe a))
  , _asBest :: Maybe c
  , _asSeen :: Set a
  }

$(makeLenses ''AStarState)

initialASState :: (Num c) => a -> c -> AStarState a c
initialASState start f = AS
  { _asCameFrom = M.singleton start Nothing
  , _asOpenSet = Q.singleton start f (0, Nothing)
  , _asBest = Nothing
  , _asSeen = S.empty
  }

search
  :: forall a c. (Ord a, Ord c, Num c, Show c, Show a)
  => (a -> Map a c) -- ^ neighbourhood
  -> (a -> c)       -- ^ heuristic
  -> (a -> Bool)    -- ^ termination condition
  -> a              -- ^ start
  -> Maybe _ -- ^ perhaps the cost with the path
search neighbours heur term start = go (initialASState start (heur start))
  where
    reconstruct :: (a, Map a (Maybe a)) -> Set a
    reconstruct (goal, mp) = S.fromList $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)

    go :: AStarState a c -> Maybe (c, Set a)
    go as@AS{..} = Q.minView _asOpenSet >>= \x -> doAStar $ traceShowMsg "trace: " (x ^. _1, x ^. _2) x
      where
        doAStar (n, c, (g, p), open)
          | term n && maybe True (c ==) _asBest =
              let as' = as
                      & asOpenSet .~ open
                      & asSeen %~ S.union (reconstruct (n, M.insert n p _asCameFrom))
                      & asBest ?~ c
                      & asCameFrom %~ M.insert n p
               in traceShowMsg "term: " (n, c, _asCameFrom) $ go as'
          | maybe False (c >) _asBest = _asBest <&> (, _asSeen)
          | otherwise = let as' = as & asOpenSet .~ open & asCameFrom %~ M.insert n p
                            !ns = neighbours n
                         in go $ M.foldlWithKey' (updateNeighbour g (Just n)) as' ns

    updateNeighbour :: c -> Maybe a -> AStarState a c -> a -> c -> AStarState a c
    updateNeighbour g p as n w =
      let gScore' = g+w
      in as & asOpenSet %~ insertIfBetter n (gScore' + heur n) (gScore', p)

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q =
  case Q.lookup k q of
    Nothing       -> Q.insert k p x q
    Just (p', _)
      | p < p'    -> Q.insert k p x q
      | otherwise -> q

parse 'S' = Just Start
parse 'E' = Just End
parse '.' = Just Space
parse _ = Nothing

toPoints mp = do
    let points = M.fromListWith (<>) . map (\(k, x) -> (x, S.singleton k)) . M.toList $ mp
    [s] <- S.toList <$> M.lookup Start points
    [e] <- S.toList <$> M.lookup End points
    return (s, e, fold points)

solve :: (Point, Dir) -> Point -> Set Point -> Maybe (Int, _)
solve start end mp = search next cost ((== end) . fst) start
    where
        next (p,d) = M.fromList [((p',d'),c) | ((p',d'), c) <- [((p, d <> East), 1000), ((p, d <> West), 1000), ((p + dirVec d, d), 1)], p' `S.member` mp]
        cost = manhattan end . fst

day16a :: _ :~> _
day16a = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, mp) -> fst <$> solve (s, East) e mp
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, mp) -> S.map fst . snd <$> solve (s, East) e mp
    }
