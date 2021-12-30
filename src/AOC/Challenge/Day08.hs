{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Solver      ((:~>)(..), dyno_)
import           AOC.Common      (CharParser, parseLines, pTok, pDecimal, Point, Dir(..), dirVec')
import           Data.Set        (Set)
import           Data.Foldable   (foldl')
import           Linear          (V2(..), _x, _y, (^*))
import           Control.Lens    (view)
import           Text.Megaparsec ((<|>), choice)
import           Advent.OCR      (parseLettersWith)
import qualified Data.Set as S

data Instruction
 = Rect (V2 Int)
 | Rotate Dir Int Int
 deriving Show

parser :: CharParser Instruction
parser = choice [parseRect, parseRotate]
    where
        pN = pTok pDecimal
        parseRect = pTok "rect" *> (Rect <$> (V2 <$> (pN <* "x") <*> pN))
        parseRotate = pTok "rotate" *> (Rotate <$> pDir <*> pN <*> (pTok "by" *> pN))
            where
                pDir = (South <$ "column x=") <|> (East <$ "row y=")

solve :: V2 Int -> [Instruction] -> Set Point
solve bounds = foldl' solve' S.empty
    where
        solve' screen (Rect (V2 w h)) = foldr S.insert screen (V2 <$> [0..w-1] <*> [0..h-1])
        solve' screen (Rotate d idx amt) = S.map (move d) screen
            where
                move South p@(V2 x _) | idx == x = mod <$> p + (dirVec' South ^* amt) <*> bounds
                move East  p@(V2 _ y) | idx == y = mod <$> p + (dirVec' East ^* amt)  <*> bounds
                move _     p                     = p

-- ^ 'hack' adds a space between letters in the output so the OCR can pick them up
-- This is totally legit
hack :: Set Point -> Set Point
hack = S.map shift
    where
        shift (V2 c r) | c >= 20    = V2 (c+1) r
                       | otherwise = V2 c r

day08a :: [Instruction] :~> Int
day08a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . S.size . solve (V2 (dyno_ "w" 50) (dyno_ "h" 6))
    }

day08b :: [Instruction] :~> String
day08b = MkSol
    { sParse = parseLines parser
    , sShow  = id
    , sSolve = parseLettersWith (view _x) (view _y) . hack . solve (V2 (dyno_ "w" 50) (dyno_ "h" 6))
    }
