module AOC.Common.Search
  ( aStar
  , aStar'
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Map       (Map)
import           Data.OrdPSQ    (OrdPSQ)
import qualified Data.Map       as M
import qualified Data.OrdPSQ    as Q

data AStarState a c = AS
  { _asCameFrom :: Map a (Maybe a)
  , _asOpenSet :: OrdPSQ a c (c, Maybe a)
  }

$(makeLenses ''AStarState)

initialState :: (Num c) => a -> c -> AStarState a c
initialState start f = AS
  { _asCameFrom = M.singleton start Nothing
  , _asOpenSet = Q.singleton start f (0, Nothing)
  }

-- | Fast(er) version of [aStar from search-algorithms](https://hackage.haskell.org/package/search-algorithms-0.3.1/docs/Algorithm-Search.html#v:aStar)
aStar
  :: forall a c. (Ord a, Ord c, Num c)
  => Map a (Map a c)  -- ^ neighbourhood
  -> (a -> c)         -- ^ heuristic
  -> (a -> Bool)      -- ^ termination condition
  -> a                -- ^ start
  -> Maybe (c, [a])   -- ^ perhaps the cost with the path
aStar neighbours heur term start = (,[]) <$> aStar'' (initialState start (heur start))
  where
    aStar'' :: AStarState a c -> Maybe c
    aStar'' as@AS{..} = Q.minView _asOpenSet >>= doAStar
      where
        doAStar (n, c, (g, p), open)
          | term n = Just c
          | otherwise = let as' = as & asOpenSet .~ open & asCameFrom %~ M.insert n p
                            ns = neighbours M.! n `M.difference` _asCameFrom
                         in aStar'' $ M.foldlWithKey' (updateNeighbour g (Just n)) as' ns

    updateNeighbour :: c -> Maybe a -> AStarState a c -> a -> c -> AStarState a c
    updateNeighbour g p as n w =
      let gScore' = g + w
      in as & asOpenSet %~ insertIfBetter n (gScore' + heur n) (gScore', p)

aStar'
  :: forall a c. (Ord a, Ord c, Num c)
  => (a -> Map a c) -- ^ neighbourhood
  -> (a -> c)       -- ^ heuristic
  -> (a -> Bool)    -- ^ termination condition
  -> a              -- ^ start
  -> Maybe (c, [a]) -- ^ perhaps the cost with the path
aStar' neighbours heur term start = second reconstruct <$> aStar'' (initialState start (heur start))
  where
    reconstruct :: (a, Map a (Maybe a)) -> [a]
    reconstruct (goal, mp) = reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)

    aStar'' :: AStarState a c -> Maybe (c, (a, Map a (Maybe a)))
    aStar'' as@AS{..} = Q.minView _asOpenSet >>= doAStar
      where
        doAStar (n, c, (g, p), open)
          | term n = Just (c, (n, M.insert n p _asCameFrom))
          | otherwise = let as' = as & asOpenSet .~ open & asCameFrom %~ M.insert n p
                            ns = neighbours n `M.difference` _asCameFrom
                         in aStar'' $ M.foldlWithKey' (updateNeighbour g (Just n)) as' ns

    updateNeighbour :: c -> Maybe a -> AStarState a c -> a -> c -> AStarState a c
    updateNeighbour g p as n w =
      let gScore' = g + w
      in as & asOpenSet %~ insertIfBetter n (gScore' + heur n) (gScore', p)


insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case Q.lookup k q of
    Nothing       -> Q.insert k p x q
    Just (p', _)
      | p < p'    -> Q.insert k p x q
      | otherwise -> q
