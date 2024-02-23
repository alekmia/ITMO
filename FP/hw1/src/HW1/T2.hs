module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z n = n
nplus (S m) n = S $ nplus m n

nmult :: N -> N -> N
nmult Z n = Z
nmult (S m) n = nplus (nmult m n) n

nsub :: N -> N -> Maybe N
nsub n Z = Just n
nsub Z n = Nothing
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z n = LT
ncmp n Z = GT
ncmp (S n) (S m) = ncmp n m

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = nToNum n + 1

nEven :: N -> Bool
nEven = undefined

nOdd :: N -> Bool
nOdd = undefined

ndiv :: N -> N -> N
ndiv = undefined

nmod :: N -> N -> N
nmod = undefined
