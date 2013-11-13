module Course.Core(
  Eq(..)
, Ord(..)
, Show(..)
, Enum(..)
, Integral(..)
, Bounded(..)
, Num(..)
, Bool(..)
, Either(..)
, Int
, Integer
, IO
, Rational
, seq
, error
, undefined
, const
, flip
, id
, (.)
, ($)
, (&&)
, (||)
, not
, even
, odd
, fst
, snd
, reads
, getChar
, on
, IsString(..)
, module Data.Char
) where


import Prelude(
    Eq(..)
  , Ord(..)
  , Show(..)
  , Enum(..)
  , Integral(..)
  , Bounded(..)
  , Num(..)
  , Bool(..)
  , Either(..)
  , Char
  , Int
  , Integer
  , IO
  , Rational
  , seq
  , error
  , undefined
  , const
  , flip
  , id
  , (.)
  , ($)
  , (&&)
  , (||)
  , not
  , even
  , odd
  , fst
  , snd
  , reads
  )
import Data.String(
  IsString(..)
  )

import System.IO(
    getChar
  )
import Data.Function(
    on
  )
import Data.Char
