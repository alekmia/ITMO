module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad
import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept f (Success a) = Success (f a)

-- | Changes content of given ExceptState by applying given function
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f a = ES (mapExcept (mapAnnotated f) . runES a)

-- | Wraps value into ExceptedState
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

runJES :: ExceptState e s (ExceptState e s a) -> s -> Except e (Annotated s a)
runJES a s = case runES a s of
    Error e -> Error e
    Success (a1 :# e) -> runES a1 e

-- | Joins two nested ExceptStates
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState a = ES (runJES a)

-- | Modifies the excepted state by given function
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

-- | Creates ExceptedState that always throws error
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  (>>=) m f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero
  deriving Show

binaryOper :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double)
  -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binaryOper f g a b  = do
  x <- eval a
  y <- eval b
  modifyExceptState $ \s -> f x y : s
  return (g x y)

unaryOper :: (Double -> Prim Double) -> (Double -> Double)
  -> Expr -> ExceptState EvaluationError [Prim Double] Double
unaryOper f g a = do
  x <- eval a
  modifyExceptState $ \s -> f x : s
  return (g x)

-- | Evaluates expression. 
-- 
-- Takes in Expression and returns either 'Error' (when dividing by 0), 
-- or 'Success' with the evaluated value and trace  
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a) = return a
eval (Op (Add a b)) = binaryOper Add (+) a b
eval (Op (Sub a b)) = binaryOper Sub (-) a b
eval (Op (Mul a b)) = binaryOper Mul (*) a b

eval (Op (Div a b)) = do
  x <- eval a
  y <- eval b
  if y == 0 then throwExceptState DivideByZero
  else modifyExceptState $ \s -> Div x y : s
  return (x / y)

eval (Op (Abs a)) = unaryOper Abs abs a
eval (Op (Sgn a)) = unaryOper Sgn signum a

