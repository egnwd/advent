{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    day18a
  , day18b
                           , SnailNumF(..)
                           , explode
                           , split
                           , applySnailRules
                           , parsePair
  ) where

import           AOC.Prelude hiding (many)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import Control.Comonad.Cofree
import Control.Comonad
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S

data SnailNum = SN Int | SP SnailNum SnailNum deriving (Eq)

$(makeBaseFunctor ''SnailNum)

instance Show SnailNum where
    show (SN n) = show n
    show (SP l r) = show [l,r]

(@+) :: SnailNum -> SnailNum -> SnailNum
a @+ b = fixedPoint applySnailRules $ SP a b

parsePair, parseNumber :: CharParser SnailNum
parsePair = between (char '[') (char ']') (SP <$> (parseNumber <|> parsePair) <*> (pTok (char ',') *> (parseNumber <|> parsePair)))
parseNumber = SN <$> pDecimal

applySnailRules n = let (e, f) = explode n
                     in if f then e else split n

split :: SnailNum -> SnailNum
split = snd . histo go
    where
        go :: SnailNumF (Cofree SnailNumF (Bool, SnailNum)) -> (Bool, SnailNum)
        go (SNF n) | n >= 10 = let (l, r) = split' n in (True, SP (SN l) (SN r))
                   | otherwise = (False, SN n)
        go (SPF l r) = case extract l of
                           (True, n) -> (True, SP n (out r))
                           (False, n) -> let (f, n) = extract r in (f, SP (out l) n)

split' :: Int -> (Int, Int)
split' n = let n' = (fromIntegral n / 2) :: Double
                in (floor n', ceiling n')

out :: Cofree SnailNumF a -> SnailNum
out (_ :< SNF n) = SN n
out (_ :< SPF l r) = SP (out l) (out r)

explode :: SnailNum -> (SnailNum, Bool)
explode inN = let (_, sn, f) = histo go inN 0 in (sn, f)
    where
        go :: SnailNumF (Cofree SnailNumF (Int -> (Maybe (Either Int Int), SnailNum, Bool))) -> Int -> (Maybe (Either Int Int), SnailNum, Bool)
        go (SNF n) d = (Nothing, SN n, False)
        go (SPF (_ :< SPF (_ :< SNF nl) (_ :< SNF nr)) r) 3 = let r' = out r in (Just (Left nl), SP (SN 0) (addLeftmost r' nr), True)
        go (SPF l (_ :< SPF (_ :< SNF nl) (_ :< SNF nr))) 3 = let l' = out l in (Just (Right nr), SP (addRightmost l' nl) (SN 0), True)
        go (SPF l r) d
          = let l' = out l
                r' = out r
             in case extract l (d+1) of
                  (Nothing, sn, True) -> (Nothing, SP sn r', True)
                  (Nothing, sn, False) -> case extract r (d+1) of
                                     (Nothing, sn, f) -> (Nothing, SP l' sn, f)
                                     (Just (Left nl), sn, f) -> (Nothing, SP (addRightmost l' nl) sn, f)
                                     (rnr, sn, f) -> (rnr, SP l' sn, f)
                  (Just (Right nr), sn, f) -> (Nothing, SP sn (addLeftmost r' nr), f)
                  (lnl, sn, f) -> (lnl, SP sn r', f)


addLeftmost, addRightmost :: SnailNum -> Int -> SnailNum
addLeftmost n r = para go n
    where
        go :: SnailNumF (SnailNum, SnailNum) -> SnailNum
        go (SNF n) = SN (n+r)
        go (SPF (_origL, recurL) (origR, _recurR)) = SP recurL origR

addRightmost n r = para go n
    where
        go :: SnailNumF (SnailNum, SnailNum) -> SnailNum
        go (SNF n) = SN (n+r)
        go (SPF (origL, _) (_, recurR)) = SP origL recurR

magnitude :: SnailNum -> Int
magnitude = cata go
    where
        go (SNF n) = n
        go (SPF l r) = (3 * l) + (2 * r)

solve (n:|ns) = map magnitude $ concat [[x @+ y, y @+ x] | (x:ys) <- tails (n:ns), y <- ys]

{-
[1,1]
[2,2]
[3,3]
[4,4]
>>> [[[[1,1],[2,2]],[3,3]],[4,4]]
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
>>> [[[[3,0],[5,3]],[4,4]],[5,5]]
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
>>> [[[[5,0],[7,4]],[5,5]],[6,6]]
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
>>> [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
[2,9]
>>> [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
>>> [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
-}

day18a :: _ :~> _
day18a = MkSol
    { sParse = nonEmpty <=< traverse (Just . parseOrFail parsePair) . lines
    , sShow  = show
    , sSolve = Just . magnitude . foldl1 (@+)
    }

day18b :: _ :~> _
day18b = MkSol
    { sParse = sParse day18a
    , sShow  = show
    , sSolve = Just . maximum . solve
    }
