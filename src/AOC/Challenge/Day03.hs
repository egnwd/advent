{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude
import Linear.V2
import qualified Data.Map as M
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char  as MP
import qualified Data.Set as S

data Movement = U Int | D Int | L Int | R Int deriving (Eq, Show)

toV = \case
    U n -> V2 n    0
    D n -> V2 (-n) 0
    L n -> V2 0    (-n)
    R n -> V2 0    n

parse :: CharParser ([Movement], [Movement])
parse = (,) <$> MP.sepBy parseMovement "," <* MP.newline <*> MP.sepBy parseMovement ","

parseMovement :: CharParser Movement
parseMovement = (U <$ "U" <|> D <$ "D" <|> L <$ "L" <|> R <$ "R") <*> pDecimal

solvea :: [Movement] -> [Movement] -> _
solvea w1 w2 = getMin $ foldMap (Min . manhattan (V2 0 0)) crossings
    where
        crossings = S.delete (V2 0 0) $ S.intersection w1' w2'
        w1' = pathOf w1
        w2' = pathOf w2
        pathOf = S.fromList . concat . snd . mapAccumL (\a b -> (a + toV b, init $ lineTo (V2 a (a + toV b)))) (V2 0 0)

solveb :: [Movement] -> [Movement] -> Int
solveb w1 w2 = getMin . foldMap Min . M.delete (V2 0 0) $ M.intersectionWith (+) w1' w2'
    where
        w1' = pathOf w1
        w2' = pathOf w2
        pathOf = M.fromListWith min . flip zip [0..] . concat . snd . mapAccumL (\a b -> (a + toV b, init $ lineTo (V2 a (a + toV b)))) (V2 0 0)

day03a :: _ :~> _
day03a = MkSol
    { sParse = MP.parseMaybe parse
    , sShow  = show
    , sSolve = Just . uncurry solvea
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = MP.parseMaybe parse
    , sShow  = show
    , sSolve = Just . uncurry solveb
    }
