module Advent.Compass
  ( Cardinality(..)
  , Point(..)
  , parseCardinality
  , rotate
  , origin
  , toVec
  ) where


import Advent.Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Linear.V2 hiding (rotate)
import Data.Group

type Point = V2 Int
data Cardinality = North | East | South | West deriving (Show, Enum, Bounded)

parseCardinality :: Parser Cardinality
parseCardinality = choice
  [ North <$ char 'N'
  , East  <$ char 'E'
  , South <$ char 'S'
  , West  <$ char 'W'
  ]

origin :: Point
origin = V2 0 0

rotate North v        = v
rotate East  (V2 x y) = V2 (-y) x
rotate South v        = negate v
rotate West  (V2 x y) = V2 y (-x)

rotateCardinality North = id
rotateCardinality East  = goEast
rotateCardinality South = goEast . goEast
rotateCardinality West  = goEast . goEast . goEast

toVec :: Cardinality -> Point
toVec North = V2 1 0
toVec East  = V2 0 1
toVec South = V2 (-1) 0
toVec West  = V2 0 (-1)

instance Semigroup Cardinality where
  (<>) = rotateCardinality

instance Monoid Cardinality where
  mempty = North

instance Group Cardinality where
  invert = goEast . goEast

goEast West = North
goEast c = succ c
