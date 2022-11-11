{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Prelude
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Data.Map as M
import qualified Data.Set as S

parseName :: CharParser String
parseName = P.many $ P.satisfy isAlphaNum

parseOrbit = (,) <$> parseName <* ")" <*> parseName

solvea :: Map String String -> Int
solvea m = M.foldr (+) 0 orbits
    where
        orbits = M.map (\o -> 1 + (M.findWithDefault 0 o orbits)) $ m

solveb :: Map String (S.Set String) -> _
solveb m = aStar' (os M.!) (const 1) (=="SAN") "YOU"
    where
        os = M.fromSet (const 1) <$> m

day06a :: Map String String :~> _
day06a = MkSol
    { sParse = fmap (M.fromList . map swap) . parseLines parseOrbit
    , sShow  = show
    , sSolve = Just . solvea
    }

day06b :: _ :~> _
day06b = MkSol
    { sParse = fmap (M.fromListWith S.union . map (second S.singleton) . concatMap ((:) <*> (return . swap))) . parseLines parseOrbit
    , sShow  = show
    , sSolve = fmap ((subtract 3) . fst) . solveb
    }
