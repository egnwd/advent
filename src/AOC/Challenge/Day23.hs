-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Prelude

import qualified Data.Map                       as M
import qualified Data.Set                       as S

parse :: String -> Maybe (Map String (Set String))
parse s = do
    ts <- traverse (listTup . splitOn "-") . lines $ s
    let mp = M.fromListWith (<>) . map (second S.singleton) $ ts
    let mp' = M.fromListWith (<>) . map (swap . first S.singleton) $ ts
    return $ M.unionWith (<>) mp mp'

party :: Set String -> Bool
party = not . S.null . S.filter ((== Just 't') . listToMaybe)

findTricycle :: Map String (Set String) -> _
findTricycle mp = S.size . S.filter party . fold . snd $ M.mapAccumWithKey (\seen k _ -> (S.insert k seen, go 3 k seen)) S.empty mp
    where
        expand :: String -> Set String
        expand x = mp M.! x
        go :: Int -> String -> Set String -> Set (Set String)
        go n0 x0 seen0 = dfs (n0-1) x0 seen0 S.empty
            where
                dfs 0 x _ path
                  | x0 `S.member` expand x = S.singleton $ S.insert x path
                  | otherwise = mempty
                dfs n x seen path = foldl' (\a b -> a <> dfs (n-1) b (S.insert x seen) (S.insert x path)) S.empty (expand x `S.difference` seen)

findMaximumClique :: Map String (Set String) -> _
findMaximumClique  = S.toAscList . maximumBy (compare `on` S.size) . go
    where
        go :: Map String (Set String) -> Set (Set String)
        go g = case M.minViewWithKey g of
                 Nothing -> S.singleton S.empty
                 Just ((v, ns), g') -> S.foldr' (\k -> S.insert (S.insert v (k `S.intersection` ns)) . S.insert k) S.empty (go g')


day23a :: Map String (Set String) :~> Int
day23a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . findTricycle
    }

day23b :: Map String (Set String) :~> [String]
day23b = MkSol
    { sParse = parse
    , sShow  = intercalate ","
    , sSolve = Just . findMaximumClique
    }
