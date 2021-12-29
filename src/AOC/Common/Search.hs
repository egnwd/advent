{-# LANGUAGE NamedFieldPuns #-}

module AOC.Common.Search
  ( aStar
  , aStar'
  , floydWarshall
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Tuple
import           Control.Monad.State
import           Data.Traversable
import           Data.Maybe
import           Data.Map       (Map)
import           Data.OrdPSQ    (OrdPSQ)
import qualified Data.Map       as M
import qualified Data.OrdPSQ    as Q

data AStarState a c = AS
  { _asCameFrom :: Map a (Maybe a)
  , _asOpenSet :: OrdPSQ a c (c, Maybe a)
  }

$(makeLenses ''AStarState)

initialASState :: (Num c) => a -> c -> AStarState a c
initialASState start f = AS
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
aStar neighbours heur term start = (,[]) <$> aStar'' (initialASState start (heur start))
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
aStar' neighbours heur term start = second reconstruct <$> aStar'' (initialASState start (heur start))
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
                            !ns = neighbours n `M.difference` _asCameFrom
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

data FloydWarshallState a c = FW
  { _fwNext :: Map a (Map a a)
  , _fwCosts :: Map a (Map a c)
  } deriving Show

$(makeLenses ''FloydWarshallState)

initialFWState :: (Ord a, Ord c, Num c, Show a, Show c) => Map a (Map a c) -> FloydWarshallState a c
initialFWState m = FW
  { _fwNext = initialParents
  , _fwCosts = initialCosts
  }
    where
      initialParents = initialCosts <&> imap const
      initialCosts = M.unionWith M.union <*> selfNoCost $ m
      selfNoCost = imap (const . flip M.singleton 0)

floydWarshall
  :: forall a c . (Ord a, Ord c, Num c, Bounded c, Show a, Show c)
  => Map a (Map a c) -- ^ neighbourhood
  -> Map a (Map a (Maybe (c, [a])))
floydWarshall neighbours = reconstruct . execState fw . initialFWState $ neighbours
  where
    reconstruct :: FloydWarshallState a c -> Map a (Map a (Maybe (c, [a])))
    reconstruct FW { _fwNext, _fwCosts } = _fwCosts & imap (\a -> imap (\b c -> sequence (c, paths a b)))
      where
        paths a b
          | a /= b = do
            a' <- (M.lookup b <=< M.lookup a) _fwNext
            rest <- paths a' b
            return (a':rest)
          | otherwise = Just []
    vs = M.keys neighbours
    fw = forM vs $ \k ->
           forM vs $ \i ->
             forM vs $ \j -> do
               cij <- fromMaybe maxBound <$> uses fwCosts (M.lookup j <=< M.lookup i)
               cik <- fromMaybe maxBound <$> uses fwCosts (M.lookup k <=< M.lookup i)
               ckj <- fromMaybe maxBound <$> uses fwCosts (M.lookup j <=< M.lookup k)
               when (cij > cik + ckj && cik + ckj >= 0) $ do
                 fwCosts %= M.adjust (M.insert j (cik+ckj)) i
                 nik <- uses fwNext (M.lookup k <=< M.lookup i)
                 fwNext %= M.adjust (M.alter (const nik) j) i
