module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ x Leaf = x
tfoldr f x (Branch _ l h r) = tfoldr f (f h (tfoldr f x r)) l