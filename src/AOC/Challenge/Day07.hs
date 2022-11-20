{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Prelude
import Text.Megaparsec
import Control.Lens
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

type Requirements = M.Map Char (S.Set Char)

parseRequirement :: CharParser Requirements
parseRequirement = do
    r <- pTok "Step" *> pTok anySingle
    c <- pTok "must be finished before step" *> pTok anySingle <* "can begin."
    return $ M.fromList [(c, (S.singleton r)), (r, S.empty)]

step :: (String, Requirements) -> Either String (String, Requirements)
step (agg, rs) = case minimumByOf traverse (compare `on` snd) (M.toList rs) of
            Just (c, _) -> Right (agg |> c, fmap (S.delete c) . M.delete c $ rs)
            Nothing -> Left agg

timeWithManyWorkers :: (Char -> Int) -> Int -> Requirements -> _
timeWithManyWorkers tf w rs = Just $ loopEither step (0, times)
    where
        times = M.fromSet (const Nothing) <$> rs
        reduceTime = fmap (M.filter (maybe True (>0))) . over (traverse.traverse._Just) pred
        step (t, log) = let log' = reduceTime log
                            w'' = w - S.size (foldMap (M.keysSet . M.filter isJust) log')
                            available = take w'' . fmap fst . filter (M.null . snd) . M.toAscList $ log'
                            keepIfEmpty (k, m) = k <$ guard (M.null m)
                         in case log' & M.minViewWithKey >>= keepIfEmpty >>= keepIfEmpty of
                            Just last -> Left $ t + tf last
                            Nothing -> Right (t+1, foldr' (\c -> fmap (setTime c) . M.delete c) log' available)

        setTime c = M.alter (fmap (return . fromMaybe (tf c))) c

time :: Int -> Char -> Int
time w = (+1) . (+w) . subtract (ord 'A') . ord

day07a :: _ :~> _
day07a = MkSol
    { sParse = fmap (M.unionsWith S.union) . parseLines parseRequirement
    , sShow  = id
    , sSolve = Just . loopEither step . ("",)
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = fmap (M.unionsWith S.union) . parseLines parseRequirement
    , sShow  = show
    , sSolve = timeWithManyWorkers (time (dyno_ "weight" 60)) (dyno_ "workers" 5)
    }
