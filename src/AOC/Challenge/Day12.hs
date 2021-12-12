{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Prelude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Data.Map as M

parser :: CharParser (String, String)
parser = (,) <$> many letterChar <* char '-' <*> many letterChar


findPaths paths = S.size . S.fromList $ findPaths' (S.singleton "start") (next "start") "start"
    where
        next s = map snd (filter ((==s).fst) paths) ++ map fst (filter ((==s).snd) paths)
        findPaths' _ _ "end" = [["end"]]
        findPaths' seen ns curr = do
            n <- ns
            guard $ n `S.notMember` seen || all isUpper n
            let seen' = if all isLower n then S.insert n seen else seen
            rest <- findPaths' seen' (next n) n
            pure $ curr : rest

findMoreExcitingPaths paths = S.size . S.fromList $ findPaths' (M.singleton "start" 1) (next "start") "start"
    where
        next s = map snd (filter ((==s).fst) paths) ++ map fst (filter ((==s).snd) paths)
        findPaths' _ _ "end" = [["end"]]
        findPaths' seen ns curr = do
            n <- ns
            guard $ partbPredicate seen n
            let seen' = if all isLower n then M.alter (Just . (+1) . fromMaybe 0) n seen else seen
            rest <- findPaths' seen' (next n) n
            pure $ curr : rest

partbPredicate seen "start" = False
partbPredicate seen a | all isUpper a = True
                      | otherwise = maybe True (\s -> s < 2 && all (<2) seen) (M.lookup a seen)

day12a :: _ :~> _
day12a = MkSol
    { sParse = parseLinesOrError parser
    , sShow  = show
    , sSolve = Just . findPaths
    }

day12b :: _ :~> _
day12b = MkSol
    { sParse = sParse day12a
    , sShow  = show
    , sSolve = Just . findMoreExcitingPaths
    }
