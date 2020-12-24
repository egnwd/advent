{-# LANGUAGE RankNTypes #-}
module Advent
    ( module Advent.Parsing
    , module Advent.Passport
    , fix
    ) where

import Advent.Passport
import Advent.Parsing

-- Utilities
fix f x
  | x == fx = fx
  | otherwise = fix f fx
  where fx = f x

