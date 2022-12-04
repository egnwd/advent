{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Prelude
import Linear
import qualified Data.Ix as Ix

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

parseThing :: String -> Maybe (S.Set Int, S.Set Int)
parseThing s = do
    [a,b,c,d] <- traverse readMaybe . words . clearOut (not . isDigit) $ s
    let l = S.fromList . Ix.range $ (a,b)
    let r = S.fromList . Ix.range $ (c,d)
    return (l,r)

day04a :: _ :~> _
day04a = MkSol
    { sParse = traverse parseThing . lines
    , sShow  = show
    , sSolve = Just . countTrue (\(l,r) -> S.isSubsetOf l r || S.isSubsetOf r l)
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = traverse parseThing . lines
    , sShow  = show
    , sSolve = Just . countTrue (\(l,r) -> (>0) . S.size $ S.intersection l r)
    }
