module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

split :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
split splitter currChar (x :| xs)
  | splitter /= currChar = (currChar : x) :| xs
  | otherwise            = [] :| (x : xs)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn splitter = foldr (split splitter) ([] :| [])

join :: a -> [a] -> [a] -> [a]
join joiner l1 [] = joiner : l1 
join joiner l1 l2 = joiner : l1 ++ l2

joinWith :: a -> NonEmpty [a] -> [a]
joinWith joiner (x :| xs) = x ++ foldr (join joiner) [] xs
