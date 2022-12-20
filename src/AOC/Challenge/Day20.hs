{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Prelude
import Control.Lens

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

mix ns = do
    file0 <- PLC.fromList (zip [0..] ns)
    let sz = length ns
    map snd . toList <$> foldlM go file0 [0..sz-1]
        where
            go !file i = do
                x <- findFromIndex i file
                file'' <- PLC.delete x
                let (i0, n) = x ^. PLC.focus
                return (PLC.insert (i0, n) . PLC.moveN (n-1) $ file'')

findFromIndex :: Int -> PLC.PointedList (Int, a) -> Maybe (PLC.PointedList (Int, a))
findFromIndex x pl = find' ((x ==) . fst . (view PLC.focus)) $ PLC.positions pl
  where
      find' pred (PLC.PointedList a b c) =
        if pred b then Just b
                  else find pred (merge a c)
      merge []     ys = ys
      merge (x:xs) ys = x : merge ys xs

getCoords ns = do
    let vs = V.fromList ns
    let sz = V.length vs
    o <- V.elemIndex 0 vs
    let next i = (i + 1000) `mod` sz
    return $ sum . map (vs V.!) . take 3 . drop 1 . iterate next $ o

day20a :: _ :~> _
day20a = MkSol
    { sParse = traverse (readMaybe @ Int) . lines
    , sShow  = show
    , sSolve = getCoords <=< mix
    }

day20b :: _ :~> _
day20b = MkSol
    { sParse = traverse (readMaybe @ Int) . lines
    , sShow  = show
    , sSolve = Just
    }

-- mix ns = map ((file0 V.!) . snd) . sort . map swap . IM.toList $ mixed
    -- where
        -- file0 = V.fromList ns
        -- sz = V.length file0
        -- locs0 = IM.fromList . zipWith ((dupe .) . const) [0..] $ ns
        -- mixed = foldl' go locs0 [0..sz-1]
        -- coords = S.fromList [0..sz-1]
        -- go locs i = locs'
            -- where
                -- locs' = IM.map move locs
                -- move = insert . delete
                    -- where
                        -- delete x
                          -- | x > src = Just (x - 1)
                          -- | x < src = Just x
                          -- | otherwise = Nothing
                        -- insert Nothing
                          -- | dst >= src = dst
                          -- | otherwise = dst + 1
                        -- insert (Just x)
                          -- | dst >= src && x >= dst = x + 1
                          -- | dst < src && x > dst = x + 1
                          -- | otherwise = x
                -- n = file0 V.! i
                -- src = locs IM.! i
                -- dst = let dst' = (src + n) `mod` sz
                       -- in if n < 0 then ((sz + dst') - 1) `mod` sz else dst'
