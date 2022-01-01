{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Prelude
import Data.Word
import           Data.ByteString       as B (unpack)
import           Data.ByteString.Char8 as BC (pack)
import           Data.ByteString.UTF8  as BS (ByteString, fromString)
import Data.Bits

encodeWord8 :: Word8 -> [Word]
encodeWord8 x = map fromIntegral [ (x .&. 0xF0) `shiftR` 4, x .&. 0xF ]

asWords = fmap fromIntegral . encodeWord8 <=< B.unpack


day14a :: _ :~> _
day14a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

day14b :: _ :~> _
day14b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
