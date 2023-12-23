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

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
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
import Linear
import Linear.V3
import Control.Lens
import Data.Interval ((<=..<=))
import qualified Data.Interval as I
import qualified Data.IntervalSet as IS

type Brick = V2 Point3D

parseBrick = V2 <$> (parsePoint <* P.char '~') <*> parsePoint
    where
        parsePoint = V3 <$> (pDecimal <* P.char ',') <*> (pDecimal <* P.char ',') <*> pDecimal

fall :: [Brick] -> [Brick]
fall = go 0 []
    where
        go _ bricks [] = bricks
        go n bricks (b : bs) = let b' = shiftDown b (max 1 $ minZ b - n - 1)
                                in if hitsNothing b' bricks && inSpace b'
                                     then go n bricks (b' : bs)
                                     else go (max n $ maxZ b) (b : bricks) bs

getSupports :: [Brick] -> _
getSupports bricks = bimap (M.unionsWith S.union) (M.unions) . unzip $ map go bricks
    where
        filled = M.fromList . concatMap (\b -> map (,b) . fillCube $ b) $ bricks
        go b = let b' = shiftDown b 1
                   supportedBy = S.delete b $ foldl' (\s c -> maybe s (`S.insert` s) (M.lookup c filled)) S.empty (fillCube b')
                in (M.fromSet (const $ S.singleton b) supportedBy, M.singleton b supportedBy)

canBeRemoved (supports, supportedBy) b = fromMaybe True $ (all ((>1) . S.size) . M.restrictKeys supportedBy) <$> M.lookup b supports

countKnockOn :: [Brick] -> _
countKnockOn bs = sum $ S.size <$> M.restrictKeys counts fallable
    where
        (supports, supportedBy) = getSupports bs
        fallable = M.keysSet $ M.filter (any ((==1) . S.size) . M.restrictKeys supportedBy) supports
        counts = foldMap (\b -> S.insert b $ M.findWithDefault mempty b counts) <$> supports

biggieSmalls bricks = S.size . S.unions $ M.filterWithKey (\k _ -> mnz == minZ k) bricks
    where
        mnz = minZ $ minimumBy (compare `on` minZ) (M.keys bricks)

shiftDown :: Brick -> Int -> Brick
shiftDown b n = b - down
    where
        down = V3 0 0 n `V2` V3 0 0 n

inSpace b = minZ b > 0
minZ (V2 (V3 _ _ z) _) = z
maxZ (V2 _ (V3 _ _ z)) = z

hitsNothing :: Brick -> [Brick] -> Bool
hitsNothing b = not . any (\b' -> any (b' `inBoundingBox`) (fillCube b))

fillCube ((V3 xmn ymn zmn) `V2` (V3 xmx ymx zmx)) = [V3 x y z | x <- [xmn..xmx], y <- [ymn..ymx], z <- [zmn..zmx]]

day22a :: _ :~> _
day22a = MkSol
    { sParse = parseLines parseBrick
    , sShow  = show
    , sSolve = Just . (countTrue =<< (canBeRemoved . getSupports)) . fall . sortOn (fmap (view _z))
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = parseLines parseBrick
    , sShow  = show
    , sSolve = Just . countKnockOn . fall . sortOn (fmap (view _z))
    }
