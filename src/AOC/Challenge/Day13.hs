{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 (
    day13a
  , day13b
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
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Control.Lens

offBy a b = S.size $ (a S.\\ b) `S.union` (b S.\\ a)

compareRocks :: (Int, Set Int) -> (Int, Set Int) -> Maybe (Int, Int)
compareRocks (r,rc) (r', rc')
  | 1 >= offBy rc rc' = Just (r,r')
  | otherwise = Nothing

isReflection n mp = (n ==) . sum . mapMaybe (\(l, r) -> offBy <$> (M.lookup l mp) <*> (M.lookup r mp))

search n xy mn mx r0 = do
    let strips = M.mapKeysWith S.union (^. (L._yx . xy)) . M.fromSet (S.singleton . (^. xy)) $ r0
    let f (l,r) = guard (isReflection n strips (zip [l,(l-1) .. mn] [r .. mx])) $> l + 1
    asum $ zipWith (\a -> f <=< compareRocks a) (M.toList strips) (tail $ M.toList strips)

findReflection :: Int -> Set Point -> Maybe _
findReflection n r0 = do
    (L.V2 (L.V2 xmn ymn) (L.V2 xmx  ymx)) <- boundingBox <$> NES.nonEmptySet r0
    (flip L.V2 0 <$> search n L._x ymn ymx r0) <|> (L.V2 0 <$> search n L._y xmn xmx r0)

day13a :: _ :~> _
day13a = MkSol
    { sParse = Just . map (parseAsciiSet (=='#')) . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . (\(L.V2 x y) -> x * 100 + y) . sum . mapMaybe (findReflection 0)
    }

day13b :: _ :~> _
day13b = MkSol
    { sParse = sParse day13a
    , sShow  = show
    , sSolve = Just . (\(L.V2 x y) -> x * 100 + y) . sum . mapMaybe (findReflection 1)
    }
