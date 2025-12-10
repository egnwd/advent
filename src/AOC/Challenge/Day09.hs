{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import Linear
import Control.Lens

biggestRectangle xs = PSQ.fromList [((x,y), negate $ manhattan x y, size x y) | (x:ys) <- tails xs, y <- ys]
    where
        size a b = product . (1+) . abs $ a - b

biggestRGRectangle xs = PSQ.fromList [((x,y), negate $ manhattan x y, size x y) | (x:ys) <- tails xs, y <- ys]
    where
        size a b = product . (1+) . abs $ a - b

createFloor = S.fromList . concat . pairwise ((lineTo .) . V2)

day09a :: [Point] :~> Int
day09a = MkSol
    { sParse = traverse (fmap (uncurry V2) . listTup <=< traverse readMaybe . splitOn ",") . lines
    , sShow  = show
    , sSolve = preview (_Just . _3) . PSQ.minView . biggestRectangle
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = sParse day09a
    , sShow  = show
    , sSolve = Just . createFloor
    }
