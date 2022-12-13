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
import           Control.DeepSeq     (NFData)
import           Control.Lens
import           Control.Monad.State
import           Data.Conduino
import           Data.Conduino.Lift
import           Control.Exception
import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Data.Typeable
import qualified Data.Conduino.Combinators as C
import           GHC.Generics        (Generic)
import Advent.OCR

data Instruction = Noop | AddX Int deriving (Eq, Show)

parseInstruction = (Noop <$ "noop") <|> (AddX <$> ("addx " *> pDecimal))

data WalkieTalkie = WT
 { wtRegX :: Int
 , wtCycles :: Int
 } deriving (Eq, Ord, Show, Generic)
instance NFData WalkieTalkie

_wtRegX :: Lens' WalkieTalkie Int
_wtRegX = lens wtRegX (\wt newX -> wt { wtRegX = newX })

_wtCycles :: Lens' WalkieTalkie Int
_wtCycles = lens wtCycles (\wt newC -> wt { wtCycles = newC })

class Monad m => MonadWalkie m where
    wtCycle  :: m ()
    wtRead   :: m Int
    wtWrite  :: Int -> m ()
    wtSignal :: m (Int, Int)

instance Monad m => MonadWalkie (StateT WalkieTalkie m) where
    wtRead = gets wtRegX
    wtWrite x = _wtRegX += x
    wtCycle = _wtCycles += 1
    wtSignal = do
        WT{..} <- get
        return $ (wtCycles, wtRegX)

instance MonadWalkie m => MonadWalkie (Pipe i o u m) where
    wtRead = lift wtRead
    wtWrite = lift . wtWrite
    wtCycle = lift wtCycle
    wtSignal = lift wtSignal

step :: MonadWalkie m => Instruction -> Pipe i _ u m ()
step i = do
    case i of
      Noop -> wtCycle *> wtSignal >>= yield
      AddX x -> do
          wtCycle
          wtSignal >>= yield
          wtCycle
          wtSignal >>= yield
          wtWrite x

stepTilTermination :: Monad m => [Instruction] -> Pipe i _ u m WalkieTalkie
stepTilTermination s = execStateP (WT 1 0) (traverse_ step s)

interestingSignals :: Monad m => Pipe _ o u m Int
interestingSignals
  = (C.drop 19 >> C.map id)
  .| (do
      C.take 1 -- 20
      C.drop 39
      C.take 1 -- 60
      C.drop 39
      C.take 1 -- 100
      C.drop 39
      C.take 1 -- 140
      C.drop 39
      C.take 1 -- 180
      C.drop 39
      C.take 1) -- 220
  .| C.foldl (+) 0

run :: [Instruction] -> Int
run s = runPipePure $ stepTilTermination s .| C.map (uncurry (*)) .| interestingSignals

drawPixel :: (Int, Int) -> (Point, Char)
drawPixel (c, x) = (loc c, p)
    where
        loc = uncurry (flip L.V2) . (`divMod` 40) . subtract 1
        pos = (c-1) `mod` 40
        p = if pos == (x-1) || pos == x || pos == (x+1)
               then '#'
               else '.'

runb :: [Instruction] -> _
runb s = runPipePure
    $ stepTilTermination s
    .| C.map drawPixel
    .| C.take 240
    .| C.sinkList

day10a :: _ :~> _
day10a = MkSol
    { sParse = parseLines parseInstruction
    , sShow  = show
    , sSolve = Just . run
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = parseLines parseInstruction
    , sShow  = maybe "UNKNOWN" id . asciiMapToLetters (S.singleton '#') . displayAsciiMap '.'
    , sSolve = Just . M.fromList . runb
    }
