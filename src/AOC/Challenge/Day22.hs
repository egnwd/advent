{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Prelude
import Linear

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

data Land = Wall | Grass deriving (Eq, Ord)

data Instruction = Move Int | Rotate Dir deriving (Show, Eq)

instance Show Land where
    show Wall = "#"
    show Grass = "."

charMap :: Char -> Maybe Land
charMap '#' = Just Wall
charMap '.' = Just Grass
charMap _   = Nothing

parsePath = P.many ((Move <$> pDecimal) <|> (Rotate <$> (West <$ P.char 'L' <|> East <$ P.char 'R')))

password :: Point -> Dir -> Int
password (V2 x y) = ((1000 * (y+1)) + (4 * (x+1)) +) . \case
    East -> 0
    South -> 1
    West -> 2
    North -> 3

getRows, getCols :: Map Point Land -> Map Int (Map Point Land)
getRows = M.mapKeysWith M.union (\(V2 _ r) -> r) . M.mapWithKey M.singleton
getCols = M.mapKeysWith M.union (\(V2 c _) -> c) . M.mapWithKey M.singleton

getTiles :: Map Int (Map Point Land) -> Map Int (Map Point Land) -> Int -> Point -> Dir -> [Point]
getTiles rows cols n loc@(V2 c r) = \case
    North -> getPath . M.toDescList $ (cols M.! c)
    East  -> getPath . M.toAscList $ (rows M.! r)
    South -> getPath . M.toAscList $ (cols M.! c)
    West  -> getPath . M.toDescList $ (rows M.! r)
    where
        getPath = map fst . takeWhile ((/=Wall).snd) . take (n+1) . dropWhile ((/=loc).fst) . cycle

findPassword (jungle, path) = do
    let startHeading = East
    (start, _) <- M.lookupMin . M.filterWithKey (\(V2 _ y) a -> Grass == a && y == 0) $ jungle
    let (end, endHeading) = foldl' go (start, startHeading) path
    return $  password end endHeading
        where
            rows = getRows jungle
            cols = getCols jungle
            findEndPoint n loc h = last $ getTiles rows cols n loc h
            go :: (Point, Dir) -> Instruction -> (Point, Dir)
            go (loc, h) (Rotate d) = (loc, h<>d)
            go (loc, h) (Move n) = (findEndPoint n loc h, h)

day22a :: (Map Point Land, [Instruction]) :~> _
day22a = MkSol
    { sParse = sequence . bimap (parseAsciiMap charMap) (parseMaybeLenient parsePath) <=< splitInput
    , sShow  = show
    , sSolve = findPassword
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

splitInput :: String -> Maybe (String, String)
splitInput s = do
    let [mp, path] = splitOn "\n\n" s
    return (mp, path)
