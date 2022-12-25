{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day25 (
    day25a
  , day25b
                           , toNum
                           , fromNum'
                           , toSnafu
  ) where

import           AOC.Prelude

import Data.Functor.Foldable

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
import qualified Data.Functor.Foldable.TH       as F

data SNAFU
 = Z        -- bottom
 | O SNAFU  -- 0
 | I SNAFU  -- 1
 | II SNAFU -- 2
 | M SNAFU  -- -
 | MM SNAFU -- =
 deriving (Eq, Ord)

$(F.makeBaseFunctor ''SNAFU)

instance Show SNAFU where
    show  Z     = ""
    show (O  s) = show s ++ "0"
    show (I  s) = show s ++ "1"
    show (II s) = show s ++ "2"
    show (M  s) = show s ++ "-"
    show (MM s) = show s ++ "="

toSnafu :: String -> Maybe SNAFU
toSnafu = foldl' (\n a -> go a <*> n) (Just Z)
    where
        go :: Char -> Maybe (SNAFU -> SNAFU)
        go '0' = Just O
        go '1' = Just I
        go '2' = Just II
        go '-' = Just M
        go '=' = Just MM
        go  _  = Nothing

sumSnafu :: [SNAFU] -> SNAFU
sumSnafu = fromNum' . sum . map toNum

toNum :: SNAFU -> Natural
toNum = cata go
    where
        go :: SNAFUF Natural -> Natural
        go = \case
            ZF -> 0
            (OF n) -> n * 5
            (IF n) -> (n * 5) + 1
            (IIF n) -> (n * 5) + 2
            (MF n) -> (n * 5) - 1
            (MMF n) -> (n * 5) - 2

foldl f a bs = foldr (\b g x -> g (f x b)) id bs a

fromNum' n = fromNum n Z

fromNum :: Natural -> SNAFU -> SNAFU
fromNum n = case n `quotRem` 5 of
              (0, 0) -> O
              (0, 1) -> I
              (0, 2) -> II
              (0, 3) -> MM . I
              (0, 4) -> M . I
              (q, 0) -> O . fromNum q
              (q, 1) -> I . fromNum q
              (q, 2) -> II . fromNum q
              (_, 3) -> MM . fromNum next
              (_, 4) -> M . fromNum next
              _ -> undefined
          where
              next = (n + 5 - 1) `div` 5
day25a :: _ :~> _
day25a = MkSol
    { sParse = traverse toSnafu . lines
    , sShow  = show
    , sSolve = Just . sumSnafu
    }

day25b :: _ :~> _
day25b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
