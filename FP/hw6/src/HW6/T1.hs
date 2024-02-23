module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, MonadSTM, MonadConc, atomically)
import Control.Concurrent.Classy.STM (TArray, TVar, newTVar, readTVar, writeTVar, readTVarConc)
import Data.Array.MArray (newArray, getBounds, readArray, writeArray, getElems)
import Data.Hashable
import Control.Monad (when)
import Data.Bifunctor (second)

-- | Capacity of our CHT at the beginning
initCapacity :: Int
initCapacity = 16

-- | Percentage of capacity, at which we should resize our CHT
loadFactor :: Double
loadFactor = 0.75

-- | A bucket of our CHT that stores elements as (key, value) 
type Bucket k v = [(k, v)]

-- | TArray of buckets
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | CHT. Can call needed methods to get its BucketsArray or size
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Create a new CHT with capacity of initCapacity 
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  buckets <- newArray (0, initCapacity - 1) ([] :: Bucket k v) >>= newTVar
  size <- newTVar 0
  return (CHT buckets size)

-- | Gets value from CHT by value. If found - returns 'Just v', else - 'Nothing'
getCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT k cnt = atomically $ do
  bucketsArray <- readTVar (chtBuckets cnt)
  tuple <- getBounds bucketsArray
  let index = hash k `mod` (snd tuple - fst tuple + 1)
  bucket <- readArray bucketsArray index
  return (getInBucket k bucket)

getInBucket :: Eq k => k -> [(k, v)] -> Maybe v
getInBucket k (x : xs) = if fst x == k then Just (snd x) else getInBucket k xs
getInBucket _ [] = Nothing

changeInBucket :: Eq k => (k, v) -> [(k, v)] -> (Bool, [(k, v)])
changeInBucket (k, v) (x : xs) = if fst x == k
  then (False, (k, v) : xs)
  else second (x :) next where
    next = changeInBucket (k, v) xs
changeInBucket (k, v) [] = (True, [(k, v)])

-- | Adds an element to our CHT. 
-- Calculates the index in BucketsArrat using 'hash k % capacity' 
-- and adds it to the corresponding bucket 
putCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT k v cnt = atomically $ do
  bucketsArray <- readTVar (chtBuckets cnt)
  tuple <- getBounds bucketsArray
  let capacity = snd tuple - fst tuple + 1
  let index = hash k `mod` capacity
  bucket <- readArray bucketsArray index
  let changes = changeInBucket (k, v) bucket
  size <- readTVar (chtSize cnt)
  let newSize = if fst changes then size + 1 else size
  writeArray bucketsArray index (snd changes)
  when (fromIntegral newSize / fromIntegral capacity > loadFactor) $ do
    newTArray <- createNewTArray bucketsArray capacity
    writeTVar (chtBuckets cnt) newTArray
  writeTVar (chtSize cnt) newSize

createNewTArray 
  :: ( MonadSTM m
     , Eq k
     , Hashable k) 
     => TArray m Int (Bucket k v) 
     -> Int 
     -> m (TArray m Int (Bucket k v))
createNewTArray bucketsArray capacity = do
    bucketsArrayToAdd <- newArray (0, capacity * 2 - 1) ([] :: Bucket k v) >>= newTVar
    newTArray <- readTVar bucketsArrayToAdd
    elements <- getElems bucketsArray
    mapM_ (mapM_ (\(key, value) -> do
      let indexToAdd = hash key `mod` (capacity * 2)
      bucketToAdd <- readArray newTArray indexToAdd
      let changesToAdd = changeInBucket (key, value) bucketToAdd
      writeArray newTArray indexToAdd (snd changesToAdd))) elements
    return newTArray

-- | Returns size of our CHT - number of elements
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cnt = do readTVarConc (chtSize cnt)