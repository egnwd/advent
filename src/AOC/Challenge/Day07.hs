{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
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
                           , totalSize, example
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
import qualified Data.Functor.Foldable                    as F
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.String
import Control.Lens

data Entry = File String Int | Dir String [Entry] deriving (Eq, Ord, Show)

$(makeBaseFunctor ''Entry)

data Command = CD String | LS deriving (Eq, Ord, Show)

data Instruction = C Command | E Entry deriving (Eq, Ord, Show)

example :: Entry
example = Dir "/"
  [ Dir "a" [ Dir "e" [ File "i" 584 ]
            , File "f" 29116
            , File "g" 2557
            , File "h.lst" 62596
            ]
  , File "b.txt" 14848514
  , File "c.dat" 8504156
  , Dir "d" [ File "j" 4060174
            , File "d.log" 8033020
            , File "d.ext" 5626152
            , File "k" 7214296
            ]
  ]

totalSize :: Entry -> Int
totalSize = sum . filter (< 100000) . snd . F.para go
    where
        go :: EntryF (Entry, (Int, [Int])) -> (Int, [Int])
        go (FileF n sz) = (sz, [])
        go (DirF n sz) = let sub = concatMap (\(e, (z,s)) -> case e of Dir _ _ -> z:s; _ -> []) $ sz
                             me = sum . map (fst . snd) $ sz
                          in (me, sub)

        getDir (File _ _) = (*0)
        getDir (Dir _ _) = id

        getFile (File _ _) = id
        getFile (Dir _ _) = (*0)

totalSpace = 70000000
need = 30000000

deleteSmallest :: Entry -> Int
deleteSmallest e = let (total, sizes) = F.para go e
                       needToFree = need - (totalSpace - total)
                    in minimum . filter (>= needToFree) $ sizes
    where
        go :: EntryF (Entry, (Int, [Int])) -> (Int, [Int])
        go (FileF n sz) = (sz, [])
        go (DirF n sz) = let sub = concatMap (\(e, (z,s)) -> case e of Dir _ _ -> z:s; _ -> []) $ sz
                             me = sum . map (fst . snd) $ sz
                          in (me, sub)

        getDir (File _ _) = (*0)
        getDir (Dir _ _) = id

        getFile (File _ _) = id
        getFile (Dir _ _) = (*0)

parseStructure :: CharParser Entry
parseStructure = do
    P.try (P.notFollowedBy ("$ cd .." <* P.newline))
    n <- pTok "$ cd" *> pWord <* P.newline
    "$ ls" <* P.newline
    files <- parseFiles
    dirs <- P.many parseStructure
    optional ("$ cd .." <* P.newline)
    return $ Dir n (files ++ dirs)
    where
        parseFiles = P.sepEndBy (P.try (P.skipManyTill parseDirectory parseFile)) P.newline <* P.skipMany parseDirectory
        parseDirectory = pTok "dir" *> pWord <* P.newline
        parseFile :: CharParser Entry
        parseFile = flip File <$> (pTok pDecimal) <*> pWord

prettyDir (FileF n sz) = "- " ++ n ++ " (file, size=" ++ show sz ++ ")"
prettyDir (DirF n es) = "- " ++ n ++ " (dir)\n" ++ (intercalate "\n" . map (intercalate "\n" . map ("  " ++) . lines) $ es)

day07a :: _ :~> _
day07a = MkSol
    { sParse = Just . parseOrFail (parseStructure <* P.eof)
    , sShow  = show
    , sSolve = Just . totalSize
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = Just . parseOrFail (parseStructure <* P.eof)
    , sShow  = show
    , sSolve = Just . deleteSmallest
    }
