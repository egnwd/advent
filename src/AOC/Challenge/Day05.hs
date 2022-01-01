-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Prelude
import Crypto.Hash.MD5
import Numeric.Lens
import Control.Lens
import Data.Bits
import Data.Word
import Data.ByteString as B (unpack)
import Data.ByteString.UTF8 as BS (ByteString, fromString)
import Data.ByteString.Char8 as BC (pack)
import qualified Data.IntMap as M

encodeWord8 :: Word8 -> [Word]
encodeWord8 x = map fromIntegral [ (x .&. 0xF0) `shiftR` 4, x .&. 0xF ]

meetsCondition :: Int -> BS.ByteString -> Bool
meetsCondition n s = all (== 0) (take half bytes) && (even n || bytes !! half < 16)
  where half = n `div` 2
        bytes = B.unpack s

findPassword :: Int -> Int -> ByteString -> Maybe ([Word], Int)
findPassword nth zs s = iterate (loopEither go) ([], 0) !? nth
    where
      go (pw, n) = let hsh = hash (s <> (pack . show $ n))
                    in if meetsCondition zs hsh
                          then Left ((encodeWord8 <=< B.unpack) hsh !! zs : pw, n+1)
                          else Right (pw, n+1)
findPasswordSecure :: Int -> Int -> ByteString -> Maybe (IntMap Word, Int)
findPasswordSecure nth zs s = iterate (loopEither go) (M.empty, 0) !? nth
    where
      go (pw, n) = let hsh = hash (s <> (pack . show $ n))
                       pos = (fmap fromIntegral . encodeWord8 <=< B.unpack) hsh !! zs
                       c = (encodeWord8 <=< B.unpack) hsh !! (zs+1)
                    in if meetsCondition zs hsh && pos < nth && pos `M.notMember` pw
                          then Left (M.insert pos c pw, n+1)
                          else Right (pw, n+1)

day05a :: ByteString :~> [Word]
day05a = MkSol
    { sParse = Just . BS.fromString
    , sShow  = toListOf (traverse . re hex . traverse)
    , sSolve = fmap (reverse . fst) . findPassword (dyno_ "nth" 8) (dyno_ "zeros" 5)
    }

day05b :: ByteString :~> [Word]
day05b = MkSol
    { sParse = Just . BS.fromString
    , sShow  = toListOf (traverse . re hex . traverse)
    , sSolve = fmap (M.elems . fst) . findPasswordSecure (dyno_ "nth" 8) (dyno_ "zeros" 5)
    }
