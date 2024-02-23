{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances  #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

-- | TSet. Is an array of Symbols 
type TSet = [Symbol]

-- | Check if element is in TSet
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains n (n : xs) = 'True
  Contains n (x : xs) = Contains n xs
  Contains _ _ = 'False

-- | Delete element from TSet
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete n (n : xs) = xs
  Delete n (x : xs) = x : Delete n xs
  Delete _ _ = '[]

-- | Add element to TSet
type family Add (name :: Symbol) (set :: TSet) :: TSet where
  Add n s = n : Delete n s