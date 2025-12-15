{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
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

type Machine = Map Int Bool
type Button = [Int]
type Joltage = Map Int Int

parseMachine :: CharParser (Machine, [Button], Joltage)
parseMachine = do
    lights <- pTok $ P.between "[" "]" $ P.many (False <$ P.char '.' <|> True <$ P.char '#')
    wirings <- P.many $ do
        pTok $ P.between "(" ")" $ P.sepBy1 pDecimal ","
    joltage <- P.between "{" "}" $ P.sepBy1 pDecimal ","
    return (M.fromList (zip [0..] lights), wirings, M.fromList (zip [0..] joltage))

fewestPresses :: Machine -> [Button] -> Maybe _
fewestPresses goal options = go (False <$ goal)
    where
        go :: Machine -> Maybe _
        go = dijkstra neighbors (==goal)
        neighbors :: Machine -> Map Machine Int
        neighbors curr = M.fromList [ (toggle curr option, 1) | option <- options ]

fewestPressesJ :: (Machine, Joltage) -> [Button] -> Maybe _
fewestPressesJ goal options = go (bimap (False <$) (0 <$) goal)
    where
        go :: (Machine, Joltage) -> Maybe _
        go = dijkstra neighbors (==goal)
        neighbors :: (Machine, Joltage) -> Map (Machine,Joltage) Int
        neighbors curr = M.fromList [ (next, 1) | option <- options, let next = pushButton curr option, isValid next ]
        isValid :: (Machine, Joltage) -> Bool
        isValid (_, j) = or $ M.mapWithKey (\k v -> j M.! k <= v) (snd goal)

pushButton :: (Machine, Joltage) -> Button -> (Machine, Joltage)
pushButton (m,j) b = (toggle m b, ramp j b)

toggle :: Machine -> Button -> Machine
toggle = foldr' (M.adjust not)

ramp :: Joltage -> Button -> Joltage
ramp = foldr' (M.adjust succ)

day10a :: _ :~> _
day10a = MkSol
    { sParse = parseLines parseMachine
    , sShow  = show
    , sSolve = fromDist . sum <=< traverse (\(m,b,_) -> fst <$> fewestPresses m b)
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = sParse day10a
    , sShow  = show
    , sSolve = traverse (\(m,b,j) -> fst <$> fewestPressesJ (m, j) b)
    }
