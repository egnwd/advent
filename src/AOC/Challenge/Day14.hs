-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Solver ((:~>)(..),  dyno_)
import           AOC.Common (CharParser, pDecimal, pTok, Point, freqs, parseLines)

import qualified Data.Map                       as M
import qualified Text.Megaparsec.Char           as P
import           Control.Monad (ap)
import           Control.Lens (minimumOf)
import           Linear (V2(..), (^*), V4(..))

parse :: CharParser (Point, V2 Int)
parse = do
    p <- V2 <$> (P.string "p=" *> pDecimal) <*> (P.char ',' *> pTok pDecimal)
    v <- V2 <$> (P.string "v=" *> pDecimal) <*> (P.char ',' *> pTok pDecimal)
    return (p, v)

move :: Int -> Int -> Int -> Point -> V2 Int -> Point
move w h s p v = let V2 x y = p + v ^* s
                  in V2 (x `mod` w) (y `mod` h)

safety :: Int -> Int -> [Point] -> Int
safety w h rs = let (top, bottom) = splitHalf (const True) (<= my) rs'
                    (topLeft, topRight) = splitHalf (<= mx) (const True) top
                    (bottomLeft, bottomRight) = splitHalf (<= mx) (const True) bottom
                 in product $ sum <$> V4 topLeft topRight bottomLeft bottomRight
                where
                    mx = w `div` 2
                    my = h `div` 2
                    rs' = M.filterWithKey (\k _ -> and $ ap (V2 (/= mx) (/= my)) k) $ freqs rs
                    splitHalf px py = M.partitionWithKey (\k _ -> and $ ap (V2 px py) k)

day14a :: _ :~> _
day14a = MkSol
    { sParse = parseLines parse
    , sShow  = show
    , sSolve = Just . safety (dyno_ "w" 101) (dyno_ "h" 103) . map (uncurry $ move (dyno_ "w" 101) (dyno_ "h" 103) 100)
    }

day14b :: _ :~> _
day14b = MkSol
    { sParse = parseLines parse
    , sShow  = show
    , sSolve = \rs ->
        let go n = map (uncurry $ move (dyno_ "w" 101) (dyno_ "h" 103) n)
            check = safety (dyno_ "w" 101) (dyno_ "h" 103)
         in fmap snd . minimumOf traverse $ map (\n -> (,n) . check $ go n rs) [8000..9000]
    }
