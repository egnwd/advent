-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Prelude

import qualified Data.Map                       as M
import qualified Data.Set                       as S

type Towel = String
type Design = String

parseTowels :: String -> Set Towel
parseTowels = S.fromList . splitOn ", "

parseDesigns :: String -> [Design]
parseDesigns = lines

isPossible :: Set Towel -> [Design] -> Int
isPossible towels designs = evalState (length <$> filterM go designs) (M.fromSet (const True) towels)
    where
        go :: Design -> State (Map Design Bool) Bool
        go [] = return True
        go a = anyM (\s -> getOrAdd (drop (length s) a) go)
             . S.toList
             . S.filter (`isPrefixOf` a)
             $ towels

possibleDesigns :: Set Towel -> [Design] -> Int
possibleDesigns towels designs = evalState (sum <$> traverse go designs) M.empty
    where
        go :: Design -> State (Map Design Int) Int
        go [] = return 1
        go a = fmap sum
             . traverse (\s -> getOrAdd (drop (length s) a) go)
             . S.toList
             . S.filter (`isPrefixOf` a)
             $ towels

getOrAdd :: (MonadState (Map k a) m, Ord k) => k -> (k -> m a) -> m a
getOrAdd key go = gets (M.lookup key) >>= \case
    Just x -> return x
    Nothing -> do
        x <- go key
        modify $ M.insert key x
        return x

day19a :: (Set Towel, [Design]) :~> Int
day19a = MkSol
    { sParse = fmap (bimap parseTowels parseDesigns) . listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . uncurry isPossible
    }

day19b :: (Set Towel, [Design]) :~> Int
day19b = MkSol
    { sParse = fmap (bimap parseTowels parseDesigns) . listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . uncurry possibleDesigns
    }
