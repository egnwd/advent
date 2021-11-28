-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.

module AOC.Challenge.Day18
  ( day18a
  , day18b
  )
where

import AOC.Common (Point, inBoundingBox, parseAsciiSet)
import AOC.Solver (dyno_, (:~>)(..))
import Control.Monad (guard)
import qualified Data.Set as S
import Linear (V2 (..))

type Rule = Int -> S.Set Point -> Point -> Bool

lightGrid :: Int -> V2 Point
lightGrid n = V2 (pure 0) (pure n -1)

neighbours :: Int -> Point -> S.Set Point
neighbours n p = S.fromList . filter (inBoundingBox (lightGrid n)) $ do
  a <- sequence $ pure [-1 .. 1]
  guard $ a /= pure 0
  pure $ p + a

countAlive :: S.Set Point -> S.Set Point -> Int
countAlive p = S.size . S.intersection p

aliveRule :: Rule
aliveRule n s x = cnt == 2 || cnt == 3
  where
    cnt = countAlive s . neighbours n $ x

deadRule :: Rule
deadRule n s x = cnt == 3
  where
    cnt = countAlive s . neighbours n $ x

variant :: Rule -> Rule
variant f n s x = x `elem` sequence (pure [0, n -1]) || f n s x

step ::
  Rule -> -- Alive Rule
  Rule -> -- Dead Rule
  Int -> -- Size of grid
  S.Set Point -> -- Initial State
  S.Set Point -- Final State
step a d n s = S.fromList . filter go $ sequence (pure [0 .. n -1])
  where
    go :: Point -> Bool
    go x
      | x `S.member` s = a n s x
      | otherwise = d n s x

day18a :: _ :~> _
day18a =
  MkSol
    { sParse = Just . parseAsciiSet (== '#'),
      sShow = show,
      sSolve = Just . S.size . (!! dyno_ "steps" 100) . iterate (step aliveRule deadRule (dyno_ "size" 100))
    }

day18b :: _ :~> _
day18b =
  MkSol
    { sParse = Just . parseAsciiSet (== '#'),
      sShow = show,
      sSolve = Just . S.size . (!! dyno_ "steps" 100) . iterate (step (variant aliveRule) (variant deadRule) (dyno_ "size" 100))
    }
