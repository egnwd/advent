-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where


import AOC.Solver    ((:~>)(..))
import Control.Monad (guard)
import Data.Char     (isNumber)
import Data.Functor  (($>))
import Data.List     (tails)
import Data.Maybe    (mapMaybe)
import Text.Read     (readMaybe)

topAndTail :: [a] -> [a]
topAndTail xs = [head xs, last xs]

day01a :: [String] :~> Int
day01a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap sum . traverse (readMaybe . topAndTail . filter isNumber)
    }

pattern One, Two, Three, Four, Five, Six, Seven, Eight, Nine :: String
pattern One <- ('o':'n':'e': _)
pattern Two <- ('t':'w':'o' : _)
pattern Three <- ('t':'h':'r':'e':'e' : _)
pattern Four <- ('f':'o':'u':'r'  : _)
pattern Five <- ('f':'i':'v':'e'  : _)
pattern Six <- ('s':'i':'x'   : _)
pattern Seven <- ('s':'e':'v':'e':'n' : _)
pattern Eight <- ('e':'i':'g':'h':'t' : _)
pattern Nine <- ('n':'i':'n':'e'  : _)

numberMap :: String -> Maybe Char
numberMap One     = Just '1'
numberMap Two     = Just '2'
numberMap Three   = Just '3'
numberMap Four    = Just '4'
numberMap Five    = Just '5'
numberMap Six     = Just '6'
numberMap Seven   = Just '7'
numberMap Eight   = Just '8'
numberMap Nine    = Just '9'
numberMap (x : _) = guard (isNumber x) $> x
numberMap []      = Nothing

day01b :: [String] :~> Int
day01b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap sum . traverse (readMaybe . topAndTail . mapMaybe numberMap . tails)
    }
