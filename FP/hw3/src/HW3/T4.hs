module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f a = S (mapAnnotated f . runS a)

wrapState :: a -> State s a
wrapState a = S (a :#)

unwrapFirstAnnotated :: Annotated e a -> a
unwrapFirstAnnotated (a :# _) = a

unwrapSecondAnnotated :: Annotated e a -> e
unwrapSecondAnnotated (_ :# e) = e

joinState :: State s (State s a) -> State s a
joinState a = S (\i -> runS (unwrapFirstAnnotated (runS a i)) (unwrapSecondAnnotated (runS a i)))

modifyState :: (s -> s) -> State s ()
modifyState f = S (\e -> () :# f e)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) f a = S (\i -> mapAnnotated (unwrapFirstAnnotated (runS f i)) (runS a i))

instance Monad (State s) where
  (>>=) a f = joinState (fmap f a)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) a b = Op (Add a b)
  (-) a b = Op (Sub a b)
  (*) a b = Op (Mul a b)
  abs a = Op (Abs a)
  signum a = Op (Sgn a)
  fromInteger a = Val (fromInteger a)

instance Fractional Expr where
  (/) a b = Op (Div a b)
  fromRational a = Val (fromRational a)

eval :: Expr -> State [Prim Double] Double
eval (Val a) = return a
eval (Op (Add a b)) = do
  x <- eval a
  y <- eval b
  modifyState $ \s -> Add x y : s
  return (x + y)

eval (Op (Sub a b)) = do
  x <- eval a
  y <- eval b
  modifyState $ \s -> Sub x y : s
  return (x - y)

eval (Op (Mul a b)) = do
  x <- eval a
  y <- eval b
  modifyState $ \s -> Mul x y : s
  return (x * y)

eval (Op (Div a b)) = do
  x <- eval a
  y <- eval b
  modifyState $ \s -> Div x y : s
  return (x / y)

eval (Op (Abs a)) = do
  x <- eval a
  modifyState $ \s -> Abs x : s
  return (abs x)

eval (Op (Sgn a)) = do
  x <- eval a
  modifyState $ \s -> Sgn x : s
  return (signum x)

