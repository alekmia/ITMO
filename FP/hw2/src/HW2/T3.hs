module HW2.T3
  ( epart
  , mcat
  ) where

maybeAdd :: Monoid a => Maybe a -> a -> a
maybeAdd Nothing a = a
maybeAdd (Just a1) a2 = a1 <> a2

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr maybeAdd mempty

maybeEpart :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
maybeEpart (Left a) (a1, b1) = (a <> a1, b1)
maybeEpart (Right b) (a1, b1) = (a1, b <> b1)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr maybeEpart (mempty, mempty)
