{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day24 (
    day24a
  , day24b
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
import Data.Finite
import Data.Bits
import Control.Lens
import Control.Lens.TH

data Instr a = AND a a | OR a a | XOR a a deriving (Show, Ord)

instance Eq a => Eq (Instr a) where
    (AND a b) == (AND c d) = (a == c && b == d) || (a == d && b == c)
    (XOR a b) == (XOR c d) = (a == c && b == d) || (a == d && b == c)
    (OR a b) == (OR c d) = (a == c && b == d) || (a == d && b == c)
    _ == _ = False

parseStartingGates :: String -> Maybe (Map String Int)
parseStartingGates = fmap M.fromList . parseLines go
    where
        go :: CharParser (String, Int)
        go = (,) <$> (pIdent <* ": ") <*> PP.binary

parseOtherGates :: String -> Maybe (Map String (Instr String))
parseOtherGates = fmap M.fromList . parseLinesOrError go
    where
        go = flip (,) <$> (parseOp <* "-> ") <*> pWord
        parseOp = do
            a <- pIdent
            op <- pTok $ P.choice [AND <$ "AND", OR <$ "OR", XOR <$ "XOR"]
            b <- pIdent
            return $ op a b

getReg :: Char -> Map String a -> Map String a
getReg r = M.filterWithKey (const . (Just r ==) . listToMaybe)

getGates :: Map String Int -> Map String (Instr String) -> Map String Int
getGates g0 gs = gates
    where
        gates = M.union g0 (M.map unpick gs)
        unpick (AND a b) = gates M.! a .&.   gates M.! b
        unpick (OR a b)  = gates M.! a .|.   gates M.! b
        unpick (XOR a b) = gates M.! a `xor` gates M.! b


binToDec :: [Int] -> Int
binToDec = foldr' (\b acc -> acc * 2 + b) 0

solve' :: Map String Int -> Map String (Instr String) -> Maybe _
solve' g0 gs = do
    c0 <- canFindHalfAdder 0 gs
    go c0 1
        where
            go c 45 = Nothing
            go c n = do
                case canFindAdder c n gs of
                  Nothing -> Just (c,n)
                  Just c' -> go c' (n+1)
    -- where
        -- findPairs = do
            -- xxory <- badXys
            -- x <- M.keys $ M.filter (==xxory) gs
            -- M.keys $ M.filter (p2 x) gs
        -- p (OR _ _) = True
        -- p (AND _ _) = True
        -- p (XOR _ _) = False
        -- p2 t (XOR a b) = a == t || b == t
        -- p2 _ _ = False
        -- badZs = filter (/= "z45") . M.keys . M.filter p $ getReg 'z' gs
        -- badXs = map (('x':) . tail) badZs
        -- badYs = map (('y':) . tail) badZs
        -- badXys = zipWith XOR badXs badYs
        -- xys = M.filter p3 gs
        -- p3 (XOR a b) = let xy = (XOR `on` listToMaybe) a b
                        -- in xy == (Just 'x' `XOR` Just 'y') || xy == (Just 'y' `XOR` Just 'x')
        -- p3 _ = False

genReg :: Char -> Int -> String
genReg = printf "%c%02d"

findReg p = fmap fst . preview singleItem . M.toAscList . M.filter (== p)

canFindHalfAdder n gs = do
    s <- findReg (genReg 'x' n `XOR` genReg 'y' n) gs
    c <- findReg (genReg 'x' n `AND` genReg 'y' n) gs
    guard (s == genReg 'z' n)
    return c

canFindAdder c n gs = do
    s1 <- findReg (genReg 'x' n `XOR` genReg 'y' n) gs
    c1 <- findReg (genReg 'x' n `AND` genReg 'y' n) gs
    s  <- findReg (c `XOR` s1) gs
    c2  <- findReg (c `AND` s1) gs
    c  <- findReg (c1 `OR` c2) gs
    guard (s == genReg 'z' n)
    return c

day24a :: (Map String Int, Map String (Instr String)) :~> _
day24a = MkSol
    { sParse = (sequenceTuple . bimap parseStartingGates parseOtherGates) <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . binToDec . M.elems . getReg 'z' . uncurry getGates
    }

day24b :: _ :~> _
day24b = MkSol
    { sParse = (sequenceTuple . bimap parseStartingGates parseOtherGates) <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . uncurry solve'
    }
