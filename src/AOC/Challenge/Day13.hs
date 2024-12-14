-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseMaybeLenient, Point, CharParser, pDecimal)

import           Control.Lens                   ((^.))
import           Control.Monad                  (guard)
import           Data.Distributive              (distribute)
import           Data.Functor                   (($>))
import           Data.List.Split                (splitOn)
import           Data.Maybe                     (mapMaybe)
import           Linear                         (V2(..), (^*), V3(..), _x)
import qualified Text.Megaparsec.Char           as P

parse :: CharParser (V2 Int, V2 Int, Point)
parse = do
    a <- V2 <$> (P.string "Button A: X" *> pDecimal) <*> (P.string ", Y" *> pDecimal) <* P.newline
    b <- V2 <$> (P.string "Button B: X" *> pDecimal) <*> (P.string ", Y" *> pDecimal) <* P.newline
    p <- V2 <$> (P.string "Prize: X=" *> pDecimal) <*> (P.string ", Y=" *> pDecimal)
    return (a, b, p)

solve :: (V2 Int, V2 Int, Point) -> Maybe Int
solve (a,b,p) = do
    let V2 x' y' = distribute $ V3 a b p
        one = x' ^* (y' ^. _x)
        two = y' ^* (x' ^. _x)
        V3 _ b' p' = one - two
    b'' <- guard (p' `mod` b' == 0) $> p' `div` b'
    let a'' = (p ^. _x - (b ^. _x * b'')) `div` (a ^. _x)
    return $ a'' * 3 + b''

heyBigSpender :: (V2 Int, V2 Int, Point) -> (V2 Int, V2 Int, Point)
heyBigSpender (a, b, p) = (a, b, p + 10000000000000)

day13a :: [(V2 Int, V2 Int, Point)] :~> Int
day13a = MkSol
    { sParse = traverse (parseMaybeLenient parse) . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . sum . mapMaybe solve
    }

day13b :: [(V2 Int, V2 Int, Point)] :~> Int
day13b = MkSol
    { sParse = traverse (parseMaybeLenient parse) . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (solve . heyBigSpender)
    }
