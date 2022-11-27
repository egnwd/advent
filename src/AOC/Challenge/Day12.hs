{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.IntMap as M
import qualified Data.Set as S
import Text.Megaparsec.Char
import Control.Monad.Combinators

parseInitialState :: CharParser (Set Int)
parseInitialState = S.fromList . map fst . filter snd . zip [0..] <$> (pTok "initial state:" *> many (True <$ char '#' <|> False <$ char '.') <* newline)

parseInput :: CharParser (Set Int, Set String)
parseInput = do
    s0 <- parseInitialState
    newline
    ons <- S.fromList . lefts <$> (mappingParser `sepBy` newline)
    return (s0, ons)

mappingParser = do
    nh <- pWord <* pTok "=>"
    side <- (Left <$ char '#') <|> (Right <$ char '.')
    return $ side nh

step :: Set String -> Set Int -> Set Int
step from state = S.fromList state'
    where
        mn = S.findMin state
        mx = S.findMax state
        state' = filter (keepFrom from state) [mn-2..mx+2]

keepFrom from state i = S.member p from
    where
        toState True = '#'
        toState False = '.'
        p = map (toState . (`S.member` state)) [i-2..i+2]

indexedSeenBefore :: Ord a => (a -> a) -> a -> (Int, a)
indexedSeenBefore f x = go 1 (S.singleton x) x
  where
    go idx !seen !x
        | y `S.member` seen = (idx, y)
        | otherwise = go (idx+1) (S.insert y seen) y
      where
        y = f x

day12a :: _ :~> _
day12a = MkSol
    { sParse = parseMaybeLenient parseInput
    , sShow  = show
    , sSolve = \(s0, changes) -> Just . sum . head . drop (dyno_ "steps" 20) . iterate (step changes) $ s0
    }

day12b :: _ :~> _
day12b = MkSol
    { sParse = parseMaybeLenient parseInput
    , sShow  = show
    , sSolve = \(s0, changes) -> Just . indexedSeenBefore (step changes) $ s0
    }
