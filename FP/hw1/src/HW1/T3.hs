module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (M ls rs) _ _ _) = ls + rs + 1

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ l _ r) = max (tdepth l) (tdepth r) + 1

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember target (Branch m l h r)
  | h == target = True
  | h < target  = tmember target r
  | otherwise   = tmember target l

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a t
  | tmember a t = t
  | otherwise   = tinsertNeeded a t

tinsertNeeded :: Ord a => a -> Tree a -> Tree a
tinsertNeeded a Leaf = Branch (M 0 0) Leaf a Leaf
tinsertNeeded a (Branch (M ls rs) l h r)
  | h == a = Branch (M ls rs) l h r
  | h < a  = Branch (M ls (rs + 1)) l h (tinsert a r)
  | otherwise = Branch (M (ls + 1) rs) (tinsert a l) h r

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
