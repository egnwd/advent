{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
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
import           Control.Lens (preview)
import           Data.Finite

data File = F
    { _fStart :: Int
    , _fEnd :: Int
    , _fId :: Int
    } deriving (Eq, Ord, Generic)

instance NFData File

data MemoryUnit = File File | Gap Int Int deriving (Eq, Ord, Generic)

instance NFData MemoryUnit

type Memory = Seq MemoryUnit

shiftTo :: Int -> MemoryUnit -> MemoryUnit
shiftTo s (Gap s' e') = Gap s (s+e'-s')
shiftTo s (File (F s' e' i)) = File (F s (s+e'-s') i)

magicZip :: [Maybe a] -> [a]
magicZip = go <$> count <*> id <*> (reverse . catMaybes)
    where
        count = length . catMaybes
        go :: Int -> [Maybe a] -> [a] -> [a]
        go 0 _ _ = []
        go _ [] _ = []
        go _ _ [] = []
        go n (Just a:as) bs = a : go (n - 1) as bs
        go n (Nothing:as) (b:bs) = b : go (n - 1) as bs

fileCompact :: Memory -> _
fileCompact mem = M.foldr place mem files
    where
        files = foldr go M.empty mem
        go (Gap _ _) = id
        go (File f@(F _ _ i)) = M.insert i f

place :: File -> Memory -> Memory
place _ Empty = Empty
place f' (g@(Gap s e) :<| (as :|> f@(File f'')))
  | f' /= f'' = place f' (g :<| as) :|> f
  | memorySize g > memorySize f = shiftTo s f :<| Gap (s + memorySize f) e :<| (as :|> Gap (_fStart f'') (_fEnd f''))
  | memorySize g == memorySize f = shiftTo s f :<| (as :|> Gap (_fStart f'') (_fEnd f''))
  | otherwise = g :<| place f' (as :|> f)
place f' (g@(Gap _ _) :<| (as :|> g'@(Gap _ _))) = place f' (g :<| as) :|> g'
place f' (a :<| as) = a :<| place f' as

memorySize :: MemoryUnit -> Int
memorySize (Gap s e) = e - s + 1
memorySize (File (F s e _)) = e - s + 1

checksum = sum . zipWith (*) [0..]

checksumMemory :: Foldable f => f MemoryUnit -> _
checksumMemory = foldr go 0
    where
        go (Gap _ _) acc = acc
        go (File (F s e i)) acc = (i * (s+e)*(e-s+1) `div` 2) + acc

generate :: [Int] -> [Maybe Int]
generate = go True 0
    where
        go :: Bool -> Int -> [Int] -> [Maybe Int]
        go _ _ [] = []
        go flag n (x : xs) = replicate x (n <$ guard flag) <> go (not flag) (if flag then succ n else n) xs

generateMemory :: [Int] -> Memory
generateMemory = Seq.fromList . go True 0 0
    where
        go _ _ _ [] = []
        go True n s (x : xs) = File (F s (s+x-1) n) : go False (n+1) (s+x) xs
        go False n s (x : xs) = Gap s (s+x-1) : go True n (s+x) xs

day09a :: _ :~> _
day09a = MkSol
    { sParse = traverse (fmap (fromInteger . getFinite) . preview decDigit)
    , sShow  = show
    , sSolve = Just . checksum . magicZip . generate
    }

day09b :: _ :~> _
day09b = MkSol
    { sParse = traverse (fmap (fromInteger . getFinite) . preview decDigit)
    , sShow  = show
    , sSolve = Just . checksumMemory . fileCompact  . generateMemory
    }
