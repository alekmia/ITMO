-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lLeft
  , lRight
  , lWrite
  , toList
  , lGenerator
  ) where

import Control.Comonad (Comonad (..))

-- | Stores two arrays and an element between them for easy access 
data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ l x r) = LZ (map f l) (f x) (map f r)

instance Comonad ListZipper where
  extract (LZ _ x _)= x

  duplicate = lGenerator lLeft lRight

-- | Shift the LZ left or right, making it store a new element
lLeft, lRight :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) c rs) = LZ ls l (c : rs)
lLeft lz = lz

lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz = lz

-- | Change the stored element
lWrite :: a -> ListZipper a -> ListZipper a
lWrite x (LZ ls _ rs) = LZ ls x rs

-- | Convert ListZipper to an array of elements
toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f -- "tail" will never return a bottom

-- | Generate a ListZipper from an element and a two functions - 
-- one that makes the first array, one that makes the second one 
lGenerator :: (a -> a)     -- ^ The left generator
           -> (a -> a)     -- ^ The right generator
           -> a            -- ^ The focus
           -> ListZipper a -- ^ The resulting list zipper
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)