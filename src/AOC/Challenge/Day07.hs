{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import AOC.Common.Intcode
import           AOC.Prelude
import Data.Conduino
import qualified Data.Conduino.Combinators as C

amplifiers mem = foldr ((.|) . amp) (C.map id)
    where
        -- ^ Pipe that inputs and outputs numbers (amplification amount)
        -- ^ buts results never terminates nor does the upstream
        amp :: (MonadError IErr m) => Int -> Pipe Int Int Void m Void
        amp p = yieldAndPass p .| stepTilTermination mem *> throwError IENoInput

runTrial :: Memory -> [Int] -> Maybe Int
runTrial mem ps = eitherToMaybe . runPipe
    $ yieldAndDie 0
    .| amplifiers mem ps
    .| awaitSurely

-- ^ Had to look up a lot of this stream nonsense... good learning though
runFeedbackTrial :: Memory -> [Int] -> Maybe Int
runFeedbackTrial mem ps = runPipePure
    $ untilHalt (yieldAndDie 0 .| feedbackPipe (amplifiers mem ps))
    .| C.last

day07a :: _ :~> _
day07a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> fmap getMax . foldMap (fmap Max . runTrial m) $ permutations [0..4]
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> fmap getMax . foldMap (fmap Max . runFeedbackTrial m) $ permutations [5..9]
    }
