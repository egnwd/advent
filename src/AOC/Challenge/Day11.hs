{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 (
    day11a
  , day11b
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
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Linear
import Control.Lens

solve d gs = sum $ zipWith (\x ys -> sum [manhattan x y | y <- ys]) newGalaxies (tail . tails $ newGalaxies)
    where
        Just (V2 (V2 xmn ymn) (V2 xmx ymx)) = boundingBox <$> NES.nonEmptySet gs
        xs = S.map (view _x) gs
        ys = S.map (view _y) gs
        V2 exs eys = fmap (filter (/= 0)) . sequence $ extraRows ++ extraColumns
        newGalaxies = S.toList $
            foldr' (\y -> S.mapMonotonic (\g -> if (g ^. _y) > y then g & _y %~ (+(d-1)) else g))
            (foldr' (\x -> S.mapMonotonic (\g -> if (g ^. _x) > x then g & _x %~ (+(d-1)) else g)) gs exs) eys
        extraColumns = [V2 0 y | y <- [ymn .. ymx], y `S.notMember` ys]
        extraRows = [V2 x 0 | x <- [xmn .. xmx], x `S.notMember` xs]

day11a :: _ :~> _
day11a = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . solve 2
    }

day11b :: _ :~> _
day11b = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . solve (dyno_ "d" 1000000)
    }
