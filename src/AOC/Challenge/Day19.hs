{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Prelude
import           Data.Monoid
import           Data.Bifunctor
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.HashSet (HashSet)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type Replacements = [(String, String)]
type Chemical = String

parseReplacement :: CharParser (String, String)
parseReplacement = (,) <$> (pWord <* pTok "=>") <*> (pWord <* P.space)

parseAll :: CharParser (Replacements, Chemical)
parseAll = do
    rs <- P.many (P.try parseReplacement)
    P.space
    m <- pWord
    pure (rs, m)

solve :: Replacements -> Chemical -> [Chemical]
solve rs c = do
    (t, s) <- rs
    (a, b) <- opts
    guard $ isPrefixOf t b
    pure $ replace t s a b
    where
        opts = zip (inits c) (tails c)
        replace t s a b = a <> s <> drop (length t) b

solveb :: Chemical -> Int
solveb c = symbol - rn - ar - (2 * y) - (if rn > 0 then 1 else 0)
    where
         symbol = countTrue isUpper c
         rn = countSymbol "Rn" c
         ar = countSymbol "Ar" c
         y = countTrue (=='Y') c

countSymbol :: String -> String -> Int
countSymbol _ [] = 0
countSymbol s w | s `isPrefixOf` w = 1 + countSymbol s (drop (length s) w)
                | otherwise = countSymbol s (tail w)

day19a :: _ :~> _
day19a = MkSol
    { sParse = parseMaybeLenient parseAll
    , sShow  = show
    , sSolve = Just . S.size . S.fromList . uncurry solve
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = parseMaybeLenient parseAll
    , sShow  = show
    , sSolve = Just . solveb . snd
    }
