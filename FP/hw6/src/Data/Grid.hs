{-# LANGUAGE DeriveFunctor #-}

-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gUp
  , gDown
  , gLeft
  , gRight
  , gWrite
  , gHorizontal
  , gVertical
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper

-- | Grid. Has a method, that returns a ListZipper (LZ) of LZ's
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) } deriving Functor

instance Comonad Grid where
  extract g = extract (extract (unGrid g))

  duplicate = Grid . fmap gHorizontal . gVertical

-- | Returns a grid, that can be achieved from the input grid, 
-- by moving the 'current element' up or down, 
-- which is achived by shifting the external LZ left or right. 
gUp, gDown :: Grid a -> Grid a
gUp   (Grid g) = Grid (lLeft  g)
gDown (Grid g) = Grid (lRight g)  

-- | Returns a grid, that can be achieved from the input grid,
-- by moving the 'current element' left or right,
-- which is achieved by shifting all of the internal LZs left or right
gLeft, gRight :: Grid a -> Grid a
gLeft  (Grid g) = Grid (fmap lLeft  g)
gRight (Grid g) = Grid (fmap lRight g)

-- | Change the 'current element' 
gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where
    oldLine = extract g
    newLine = lWrite x oldLine

-- | Generate a LZ of all horizontal or vertical 'current element' moves 
gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical   = lGenerator gUp   gDown