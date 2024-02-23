module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some a) = a
joinOption None = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success a) = a
joinExcept (Error e) = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1)

addLists :: List a -> List a -> List a
addLists (a :. b) c = a :. addLists b c
addLists Nil c = c

joinList :: List (List a) -> List a
joinList (a :. b) = addLists a (joinList b)
joinList Nil = Nil

unwrapFun :: Fun i a -> i -> a
unwrapFun (F f) = f

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> unwrapFun (f i) i)
