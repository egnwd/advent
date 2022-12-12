{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Prelude
import Control.Lens
import Linear.V2

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

data Move = Move Dir Int deriving (Eq, Show)

moveToInstructions :: [Move] -> [Dir]
moveToInstructions = concatMap (\(Move d n) -> replicate n d)

parseMove = Move <$> pTok pDir <*> pDecimal
    where
        pDir = North <$ "D" <|> East <$ "R" <|> South <$ "U" <|> West <$ "L"

step :: ([Point], _) -> Dir -> ([Point], _)
step (rope,seen) (dirVec->d) = (rope',seen')
    where
        next = first Just . dupe
        slideRope Nothing r = next $ r+d
        slideRope (Just r') r = next $ if any (>1) (abs (r'-r)) then r + (signum <$> r' - r) else r
        (Just last, rope') = mapAccumL slideRope Nothing rope
        seen' = S.insert last seen

day09 :: Int -> _ :~> _
day09 knots = MkSol
    { sParse = parseLines parseMove
    , sShow  = show
    , sSolve = fmap S.size . preview _2 . foldl' step (replicate knots 0, S.singleton 0) . moveToInstructions
    }

day09a :: _ :~> _
day09a = day09 2

day09b :: _ :~> _
day09b = day09 10
