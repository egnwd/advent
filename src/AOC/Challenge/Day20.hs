-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Solver ((:~>)(..))
import           Data.Sequence (Seq)
import           Text.Read
import           Data.Maybe
import           Data.Foldable
import qualified Data.Sequence as Seq

mix :: Seq (Int, Int) -> Seq (Int, Int)
mix ns = foldl' go ns [1..sz]
        where
            sz = Seq.length ns
            go :: Seq.Seq (Int, Int) -> Int -> Seq.Seq (Int, Int)
            go !file i = fromMaybe file $ do
                idx <- Seq.findIndexL ((==i) . fst) file
                let file' = Seq.deleteAt idx file
                (_, n) <- file Seq.!? idx
                let idx' = (idx + n) `mod` (sz-1)
                return $ Seq.insertAt idx' (i, n) file'

getCoords :: Seq Int -> Maybe Int
getCoords ns = do
    let sz = Seq.length ns
    o <- Seq.elemIndexL 0 ns
    let next i = (i + 1000) `mod` sz
    fmap sum . traverse (ns Seq.!?) . take 3 . drop 1 . iterate next $ o

decryptionKey :: Int
decryptionKey = 811589153

day20a :: [Int] :~> Int
day20a = MkSol
    { sParse = traverse (readMaybe @ Int) . lines
    , sShow  = show
    , sSolve = getCoords . fmap snd . mix . Seq.fromList . zip [1..]
    }

day20b :: [Int] :~> Int
day20b = MkSol
    { sParse = traverse (readMaybe @ Int) . lines
    , sShow  = show
    , sSolve = (getCoords . fmap snd) . head . drop 10 . iterate mix . Seq.fromList . zip [1..] . map (*decryptionKey)
    }
