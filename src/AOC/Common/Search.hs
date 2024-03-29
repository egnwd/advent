{-# LANGUAGE NamedFieldPuns #-}

module AOC.Common.Search
  ( aStar
  , aStar'
  , beamAStar'
  , floydWarshall
  , bfs
  , bfsAll
  , binarySearch
  ) where

import           Control.Lens hiding (Empty)
import           Data.Bifunctor
import           Control.Monad.State
import           Data.Maybe
import           Data.Map       (Map)
import           Data.Set       (Set)
import           Data.Sequence  (Seq(..))
import           Data.OrdPSQ    (OrdPSQ)
import qualified Data.Map       as M
import qualified Data.OrdPSQ    as Q
import qualified Data.Sequence  as Seq
import qualified Data.Set       as S

import Debug.Trace

data AStarState a c = AS
  { _asCameFrom :: !(Map a (Maybe a))
  , _asOpenSet :: !(OrdPSQ a c (c, Maybe a))
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
                            !ns = neighbours M.! n `M.difference` _asCameFrom
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
aStar' neighbours heur term start = second reconstruct <$> go (initialASState start (heur start))
  where
    reconstruct :: (a, Map a (Maybe a)) -> [a]
    reconstruct (goal, mp) = reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)

    go :: AStarState a c -> Maybe (c, (a, Map a (Maybe a)))
    go as@AS{..} = Q.minView _asOpenSet >>= doAStar
      where
        doAStar (n, c, (g, p), open)
          | term n = Just (c, (n, M.insert n p _asCameFrom))
          | otherwise = let as' = as & asOpenSet .~ open & asCameFrom %~ M.insert n p
                            !ns = neighbours n `M.difference` _asCameFrom
                         in go $ M.foldlWithKey' (updateNeighbour g (Just n)) as' ns

    updateNeighbour :: c -> Maybe a -> AStarState a c -> a -> c -> AStarState a c
    updateNeighbour g p as n w =
      let gScore' = g+w
      in as & asOpenSet %~ insertIfBetter n (gScore' + heur n) (gScore', p)

beamAStar'
  :: forall a c. (Ord a, Ord c, Num c, Show c)
  => Int            -- ^ beam size
  -> (a -> Map a c) -- ^ neighbourhood
  -> (a -> c)       -- ^ heuristic
  -> (a -> Bool)    -- ^ termination condition
  -> a              -- ^ start
  -> Maybe (c, [a]) -- ^ perhaps the cost with the path
beamAStar' sz neighbours heur term start = second reconstruct <$> go (initialASState start (heur start))
  where
    reconstruct :: (a, Map a (Maybe a)) -> [a]
    reconstruct (goal, mp) = reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)

    go :: AStarState a c -> Maybe (c, (a, Map a (Maybe a)))
    go as@AS{..} = Q.minView (Q.fromList . take sz . Q.toAscList $ _asOpenSet) >>= doAStar
      where
        doAStar (n, c, (g, p), open)
          | term n = Just (c, (n, M.insert n p _asCameFrom))
          | otherwise = let as' = as & asOpenSet .~ open & asCameFrom %~ M.insert n p
                            !ns = neighbours n `M.difference` _asCameFrom
                         in go $ M.foldlWithKey' (updateNeighbour g (Just n)) as' ns

    updateNeighbour :: Show c => c -> Maybe a -> AStarState a c -> a -> c -> AStarState a c
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

data BFSState n = BS { _bsClosed  :: !(Map n (Maybe n))  -- ^ map of item to "parent"
                     , _bsOpen    :: !(Seq n          )  -- ^ queue
                     , _bsFound   :: !(Map n n        )  -- ^ found items
                     }

-- | Breadth-first search, with loop detection
bfs :: forall n. Ord n
    => (n -> Set n)   -- ^ neighborhood
    -> n              -- ^ start
    -> (n -> Bool)    -- ^ target
    -> Maybe [n]      -- ^ the shortest path, if it exists
bfs ex x0 dest = reconstruct <$> go (addBack x0 Nothing (BS M.empty Seq.empty M.empty))
  where
    reconstruct :: (n, Map n (Maybe n)) -> [n]
    reconstruct (goal, mp) = drop 1 . reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (mp M.! n)
    go :: BFSState n -> Maybe (n, Map n (Maybe n))
    go BS{..} = case _bsOpen of
      Empty    -> Nothing
      n :<| ns
        | dest n    -> Just (n, _bsClosed)
        | otherwise -> go . S.foldl' (processNeighbor n) (BS _bsClosed ns _bsFound) $ ex n
    addBack :: n -> Maybe n -> BFSState n -> BFSState n
    addBack x up BS{..} = BS
      { _bsClosed = M.insert x up _bsClosed
      , _bsOpen   = _bsOpen :|> x
      , _bsFound  = _bsFound
      }
    processNeighbor :: n -> BFSState n -> n -> BFSState n
    processNeighbor curr bs0@BS{..} neighb
      | neighb `M.member` _bsClosed = bs0
      | otherwise                   = addBack neighb (Just curr) bs0

-- | Breadth-first search, with loop detection, to find all matches.
bfsAll :: forall n. (Ord n)
    => (n -> Set n)             -- ^ neighborhood
    -> n                        -- ^ start
    -> (n -> Maybe n)           -- ^ keep me when True
    -> (Set n -> Bool)          -- ^ stop when True
    -> Map n [n]
bfsAll ex x0 isGood stopper = reconstruct <$> founds
  where
    (founds, parentMap) = go . addBack x0 Nothing $ BS M.empty Seq.empty M.empty
    reconstruct :: n -> [n]
    reconstruct goal = drop 1 . reverse $ goreco goal
      where
        goreco n = n : maybe [] goreco (parentMap M.! n)
    go :: BFSState n -> (Map n n, Map n (Maybe n))
    go BS{..} = case _bsOpen of
      Empty    -> (_bsFound, _bsClosed)
      (!n) :<| ns ->
        let (found', updated) = case isGood n of
              Just x
                | x `M.notMember` _bsFound -> (M.insert x n _bsFound, True)
              _   -> (_bsFound, False)
            stopHere = updated && stopper (M.keysSet found')
        in  if stopHere
              then (found', _bsClosed)
              else go . S.foldl' (processNeighbor n) (BS _bsClosed ns found') $ ex n
    addBack :: n -> Maybe n -> BFSState n -> BFSState n
    addBack !x !up BS{..} = BS
      { _bsClosed = M.insert x up _bsClosed
      , _bsOpen   = _bsOpen :|> x
      , _bsFound  = _bsFound
      }
    processNeighbor :: n -> BFSState n -> n -> BFSState n
    processNeighbor !curr bs0@BS{..} neighb
      | neighb `M.member` _bsClosed = bs0
      | otherwise                   = addBack neighb (Just curr) bs0

binarySearch :: Integral a => a -> a -> (a -> Bool) -> a
binarySearch lo hi p
    | lo == hi = lo
    | p mid = binarySearch lo mid p
    | otherwise = binarySearch (mid+1) hi p
  where
      mid = (lo + hi) `div` 2

