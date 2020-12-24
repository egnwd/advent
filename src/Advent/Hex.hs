module Advent.Hex
  ( Cardinality(..)
  , Point(..)
  , parseCardinality
  , origin
  , toVec
  , allDirections
  ) where


import Advent.Parsing
import Linear.V2 hiding (rotate)
import Text.Megaparsec
import qualified Data.Set as S

type Point = V2 Int
data Cardinality = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Show, Enum, Bounded)

parseCardinality :: Parser Cardinality
parseCardinality = choice
  [ East <$ "e"
  , SouthEast <$ try "se"
  , SouthWest <$ "sw"
  , West <$ "w"
  , NorthWest <$ try "nw"
  , NorthEast <$ "ne"
  ]

origin :: Point
origin = V2 0 0

allDirections :: S.Set Point
allDirections = S.fromList . map toVec $ [minBound..maxBound]

toVec :: Cardinality -> Point
toVec East      = V2 1 0
toVec SouthEast = V2 0 1
toVec SouthWest = V2 (-1) 1
toVec West      = V2 (-1) 0
toVec NorthWest = V2 0 (-1)
toVec NorthEast = V2 1 (-1)
