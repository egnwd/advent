{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude
import Text.Megaparsec.Char
import Control.Monad.Combinators ((<|>))
import qualified Data.Set as S
import Linear.V2
import Control.Lens.TH
import Control.Monad.State
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Lens

data Compass = North | East | South | West deriving (Eq, Ord, Show)

go North = V2 1    0
go East  = V2 0    1
go South = V2 (-1) 0
go West  = V2 0    (-1)

parseDirections = many parseDirection

parseDirection =
      (North <$ char '^')
  <|> (East  <$ char '>')
  <|> (South <$ char 'v')
  <|> (West  <$ char '<')

data Globe = Globe
  { _curPos :: V2 Int
  , _visited :: S.Set (V2 Int)
  }

$(makeLenses ''Globe)

solve :: Compass -> State Globe ()
solve c = do
  new <- curPos <+= go c
  visited %= S.insert new

parta cs = views visited S.size $ execState (mapM solve cs) startGlobe
  where
    startGlobe = Globe (V2 0 0) (S.singleton (V2 0 0))

odds [] = []
odds (x:xs) = x:evens xs

evens [] = []
evens (x:xs) = odds xs

partb cs = S.size $ S.union santaGlobe roboGlobe
  where
    startGlobe = Globe (V2 0 0) (S.singleton (V2 0 0))
    santaGlobe = view visited $ execState (mapM solve (odds cs)) startGlobe
    roboGlobe  = view visited $ execState (mapM solve (evens cs)) startGlobe

day03a :: _ :~> _
day03a = MkSol
    { sParse = parseMaybeLenient parseDirections
    , sShow  = show
    , sSolve = Just . parta
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = parseMaybeLenient parseDirections
    , sShow  = show
    , sSolve = Just . partb
    }
