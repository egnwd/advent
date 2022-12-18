{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Prelude

import Control.Lens
import Control.Lens.TH
import Debug.Trace

import Control.Monad.State

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

type Name = String
data Valve = Valve Name Int [Name] deriving (Show)

parseValve :: CharParser Valve
parseValve = do
    v <- pTok "Valve" *> pTok pWord
    fr <- "has flow rate=" *> pDecimal <* pTok ";"
    ts <- (P.try "tunnels lead to valves " <|> "tunnel leads to valve ") *> (pName `P.sepBy` (pTok ","))
    return $ Valve v fr ts
        where
            pName = P.takeWhileP (Just "name") isUpper

maxOutPressure :: _ -> _
maxOutPressure vs = fst . bimap negate (map (\(a,_,t) -> (a,t))) <$> aStar' nf heur (\(_, _, t) -> t <= 0) start
    where
        flows t = M.mapWithKey (cumFlow t)
        cumFlow t n c = flow n * (max 0 (t-c-1))
        flow n = flowRates M.! n
        flowRates = M.fromList . map (\(Valve n fr _) -> (n, negate fr)) $ vs
        fw = M.map (M.map (fst . fromJust)) $ floydWarshall neighbours
        nf (n, o, t)
          = let fs = M.fromList
                      . map (\(n', c) -> ((n', S.insert n' o, max 0 (t-c-1)), (if n' `S.member` o then 0 else 1) * cumFlow t n' c))
                      . M.toList
                      . M.delete n
                      $ fw M.! n
             in if all (==0) fs then M.singleton (n,o,0) 0 else fs
        neighbours = M.fromList . map (\(Valve n _ ns) -> (n, M.fromList . map (,1) $ ns)) $ vs
        start = ("AA", S.empty, 30)
        heur (n, o, t) = h
            where
                stillClosed = sort . M.elems . M.delete n $ M.filterWithKey (\k _ -> k `S.notMember` o) flowRates
                h = cumFlow t n 0 + sum (zipWith (*) [t, t-2 .. 0] stillClosed)

day16a :: _ :~> _
day16a = MkSol
    { sParse = parseLines parseValve
    , sShow  = show
    , sSolve = maxOutPressure
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
