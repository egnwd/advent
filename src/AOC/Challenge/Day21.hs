{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
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
import           Data.Sequence                  (Seq(..))
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Data.Finite
import Control.Lens hiding (from, to)

data ControlPad = Arrow Dir | Select deriving (Eq, Ord, Generic)
instance NFData ControlPad
instance Enum ControlPad where
    toEnum 4 = Select
    toEnum n = Arrow . toEnum $ n

    fromEnum Select = 4
    fromEnum (Arrow d) = fromEnum d

instance Bounded ControlPad where
    minBound = toEnum 0
    maxBound = toEnum 4

instance Show ControlPad where
    show (Arrow North) = "^"
    show (Arrow East) = ">"
    show (Arrow South) = "v"
    show (Arrow West) = "<"
    show Select = "A"

type NumpadDigit = Finite 11

parseNumberPad :: Char -> Maybe NumpadDigit
parseNumberPad = preview unDecDigit
numberKeypad :: Map Point NumpadDigit
numberKeypad = parseAsciiMap parseNumberPad "789\n456\n123\n 0A"

parseArrowPad :: Char -> Maybe ControlPad
parseArrowPad 'A' = Just Select
parseArrowPad '^' = Just (Arrow North)
parseArrowPad '<' = Just (Arrow West)
parseArrowPad 'v' = Just (Arrow South)
parseArrowPad '>' = Just (Arrow East)
parseArrowPad _ = Nothing
arrowKeypad :: Map Point ControlPad
arrowKeypad = parseAsciiMap parseArrowPad " ^A\n<v>"

distFromSelect :: Ord a => Map Point a -> a -> a -> Maybe Int
distFromSelect mp a x = do
    let mp' = M.fromList . map swap . M.toList $ mp
    aPos <- M.lookup a mp'
    xPos <- M.lookup x mp'
    return $ manhattan aPos xPos

-- :( if it's optimal at the start that doesn't propogate up :(
paths :: (Bounded a, Ord a) => Map Point a -> Map Point (Map Point (Maybe (Int, [Point])))
paths mp = floydWarshall $ M.mapWithKey (\k _ -> M.mapMaybe (fmap (+1000) . distFromSelect mp maxBound) $ mp `M.restrictKeys` neighboursSet k) mp

numberInstructions :: Map NumpadDigit (Map NumpadDigit (Seq Dir))
numberInstructions = instructionSet numberKeypad

arrowInstructions :: Map ControlPad (Map ControlPad (Seq Dir))
arrowInstructions = instructionSet arrowKeypad

instructions :: Map Point (Map Point (Maybe (Int, [Point]))) -> Map Point (Map Point (Seq Dir))
instructions = M.mapWithKey (\f -> M.mapMaybe (go . (f :) . snd =<<))
    where
        go :: [Point] -> Maybe (Seq Dir)
        go = sequence . Seq.fromList . pairwise ((vecDir .) . subtract)

instructionSet :: (Ord a, Bounded a) => Map Point a -> Map a (Map a (Seq Dir))
instructionSet mp = M.mapKeys (mp M.!) . M.map (M.mapKeys (mp M.!)) . instructions . paths $ mp

goto :: (Ord a, Ord b) => a -> b -> Map a (Map b (Seq Dir)) -> Maybe (Seq Dir)
goto f t = M.lookup t <=< M.lookup f

moveArrow :: ControlPad -> ControlPad -> Maybe (Seq ControlPad)
moveArrow f t = (|> Select) . Seq.sort . fmap Arrow <$> goto f t arrowInstructions

move :: NumpadDigit -> NumpadDigit -> Maybe (Seq ControlPad)
move f t = (|> Select) . Seq.sort . fmap Arrow <$> goto f t numberInstructions

press :: NumpadDigit -> NumpadDigit -> Maybe _
press f t = do
    r1s <- toList <$> move f t
    r2s <- sequence $ pairwise moveArrow (Select : r1s)
    r3s <- sequence $ pairwise moveArrow (concatMap ((Select :) . toList) r2s)
    return $ concatMap toList r3s

solve :: [NumpadDigit] -> Maybe _
solve ns = fmap (concat) . sequence . pairwise press $ (10 : ns)

day21a :: [[NumpadDigit]] :~> _
day21a = MkSol
    { sParse = traverse (traverse (preview unDecDigit)) . lines
    , sShow  = show
    , sSolve = traverse solve
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
