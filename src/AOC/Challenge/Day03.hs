{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

import           AOC.Prelude hiding (indexed)

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
import Control.Lens
import Control.Lens.Indexed
import Data.Map.Lens (toMapOf)
import Linear
import Control.Arrow ((***))

symbolGetter '.' = Nothing
symbolGetter a
 | isNumber a = Nothing
 | otherwise = Just a

parseNumberMap :: String -> (Map Point Char, Map Int ([Point], Int))
parseNumberMap s = (symbolMap, result)
    where
        result = go s
        symbolMap = parseAsciiMap symbolGetter s
        go =
             (\(_, _, mp) -> mp)
           . foldl wizz (0, S.empty, M.empty)
           . concat
           . map (filter (isNumber . snd))
           . zipWith (\y -> zipWith (\x -> (,) (V2 x y)) [0..]) [0..]
           . lines
        wizz :: (Int, Set Point, Map Int ([Point], Int)) -> (Point, Char) -> (Int, Set Point, Map Int ([Point], Int))
        wizz (i, seen, mp) (V2 x y, a) = if S.member (V2 (x-1) y) seen
                                            then (i, S.insert (V2 x y) seen, M.adjust (\(points, n) -> ((V2 x y) : points, n * 10 + digitToInt a)) i mp)
                                            else (i+1, S.singleton (V2 x y), M.insert (i+1) ([V2 x y], digitToInt a) mp)

day03a :: _ :~> _
day03a = MkSol
    { sParse = Just . parseNumberMap
    , sShow  = show
    , sSolve = uncurry $ \symbolMap ->
          Just
        . sum
        . map snd
        . filter (not . M.null . nearSymbols (const True) symbolMap . fst)
        . M.elems
    }

nearSymbols :: (Char -> Bool) -> Map Point Char -> [Point] -> Map Point Char
nearSymbols p symbols = M.restrictKeys (M.filter p symbols) . S.unions . map allNeighboursSet

day03b :: _ :~> _
day03b = MkSol
    { sParse = sParse day03a
    , sShow  = show
    , sSolve = uncurry $ \symbolMap ->
          Just
        . M.foldr (\(_, gear) -> (+ gear)) 0
        . M.filter ((== 2) . fst)
        . M.foldl (\mp (k,a) -> foldl (flip $ M.alter (Just . maybe (1, a) (succ *** (* a)))) mp . M.keys . nearSymbols (== '*') symbolMap $ k) M.empty
    }
