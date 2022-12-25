{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings, DerivingStrategies #-}

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
import Linear

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
        cumFlow t n c = flow n * (max 0 (t-c-1))
        flow n = flowRates M.! n
        flowRates = M.fromList . map (\(Valve n fr _) -> (n, negate fr)) $ vs
        fw = M.map (M.map (fst . fromJust)) $ floydWarshall neighbours
        neighbours = M.fromList . map (\(Valve n _ ns) -> (n, M.fromList . map (,1) $ ns)) $ vs
        start = ("AA", S.empty, 30)
        heur (n, o, t) = cumFlow t n 0 + sum (zipWith (*) [t, t-2 .. 0] stillClosed)
            where
                stillClosed = sort . M.elems . M.delete n $ M.withoutKeys flowRates o
        nf (n, o, t)
          = let fs = M.fromList
                      . map (\(n', c) -> ((n', S.insert n' o, max 0 (t-c-1)), (if n' `S.member` o then 0 else 1) * cumFlow t n' c))
                      . M.toList
                      . M.delete n
                      $ fw M.! n
             in if all (==0) fs then M.singleton (n,o,0) 0 else fs

data SearchState = SS !(V2 Name) !(Set Name) !Int deriving stock (Generic, Show, Eq, Ord)

instance NFData SearchState

showUnit flow (SS (V2 me ele) o t) = unlines
    [ "== Minute " ++ show (27-t) ++ " =="
    , case S.size o of
        0 -> "No valves are open."
        1 -> "Valve " ++ intercalate "," (S.toList o) ++ " is open. Releasing " ++ show (sum (flow `M.restrictKeys` o)) ++ " pressure."
        _ -> "Valves " ++ intercalate "," (S.toList o) ++ " are open. Releasing " ++ show (sum (flow `M.restrictKeys` o)) ++ " pressure."
    , "You are at valve " ++ me
    , "Elephant is at valve " ++ ele
    ]

showTrace (_, []) = ""
showTrace (x, (u:us)) = showUnit x u ++ "\n\n" ++ showTrace (x, us)

maxOutPressure2 :: _ -> _
maxOutPressure2 vs = fst . first (negate . subtract (sum flowRates * 26)) <$> aStar' nf heur (\(SS _ o t) -> t <= 0 || M.null (goodFlow `M.withoutKeys` o)) start
    where
        flow n = flowRates M.! n
        flowRates = M.fromList . map (\(Valve n fr _) -> (n, fr)) $ vs
        goodFlow = M.filter (> 0) flowRates
        neighbours = M.fromList . map (\(Valve n _ ns) -> (n, ns)) $ vs
        start = SS (V2 "AA" "AA") S.empty 25
        heur (SS _ o _) = sum $ flowRates `M.withoutKeys` o
        nf :: SearchState -> Map SearchState Int
        nf (SS n0 o t)
          = let fs = [ (SS (V2 n1 n2) o' (max 0 (t-1)), sum $ flowRates `M.withoutKeys` o)
                     | (V2 (n1, o1) (n2, o2)) <- traverse options n0
                     , let o' = o1 `S.union` o2
                     ]
                options n = open n <|> next n
                open n = (n, S.insert n o) <$ guard (n `S.notMember` o && flow n /= 0)
                next n = (,o) <$> neighbours M.! n
             in M.fromList fs

day16a :: _ :~> _
day16a = MkSol
    { sParse = parseLines parseValve
    , sShow  = show
    , sSolve = maxOutPressure
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = parseLines parseValve
    , sShow  = show
    , sSolve = maxOutPressure2
    }
