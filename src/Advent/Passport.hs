{-# LANGUAGE OverloadedStrings, GADTs, DeriveGeneric, DerivingVia, StandaloneDeriving, UndecidableInstances, TemplateHaskell, DeriveAnyClass, RankNTypes #-}
module Advent.Passport
  ( Passport(..)
  , UnvalidatedPassport(..)
  , ValidatedPassport(..)
  , UnvalidatedSetter(..)
  , Eye(..)
  , Height(..)
  , byr
  , iyr
  , eyr
  , hgt
  , ecl
  , hcl
  , pid
  ) where

import Control.Lens
import Data.Finite
import Data.Monoid.OneLiner (GMonoid(..))
import Data.Semigroup (Option(..))
import GHC.Generics
import Prelude hiding (unlines)
import Refined

import qualified Barbies as B
import qualified Data.Text as T
import Text.Megaparsec  hiding (count, between)
import Text.Megaparsec.Char hiding (count)
import Data.Void

type a <-> b = Refined (FromTo a b) Int
type n ** i  = Refined (SizeEqualTo n) [i]

data Eye    = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Show)
data Height = HCm (150 <-> 193) | HIn (59 <-> 76) deriving (Show)

data Passport f =
  Passport { _byr ::  f (1920 <-> 2002)
           , _iyr ::  f (2010 <-> 2020)
           , _eyr ::  f (2020 <-> 2030)
           , _hgt ::  f Height
           , _hcl ::  f (6 ** Finite 16)
           , _ecl ::  f Eye
           , _pid ::  f (9 ** Finite 10)
        -- , _cid :: Country Code
           } deriving (Generic, B.FunctorB, B.ApplicativeB, B.TraversableB, B.ConstraintsB)
makeLenses ''Passport

deriving instance B.AllBF Show f Passport => Show (Passport f)
deriving via GMonoid (Passport f) instance B.AllBF Semigroup f Passport => Semigroup (Passport f)
deriving via GMonoid (Passport f) instance B.AllBF Monoid f Passport => Monoid (Passport f)

type UnvalidatedPassport = Passport UnvalidatedField
type UnvalidatedField = Const (Option T.Text)
type UnvalidatedSetter a = Setter' UnvalidatedPassport (UnvalidatedField a)
type ValidatedPassport = Passport Identity


