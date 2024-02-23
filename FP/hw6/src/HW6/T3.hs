module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  , showGrid
  ) where

import Control.Monad (liftM2, replicateM)
import Control.Comonad (extract, extend)
import System.Random (StdGen, random, split)
import Control.Monad.Trans.State (state, runState)

import Data.Grid
import Data.ListZipper

-- | Simulation parameters - probability of infection (0 < p < 1), 
-- incubation period, illness duration and immunity duration
data Config = Config
  { probability :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  } deriving Show

-- | States in which the cell can be. 
-- Some have an Int parameter - days left of said state 
data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

-- | Cell. Can get its state and its StdGen
data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  }

-- | A grid of cells
type Comonad19Grid = Grid Cell

neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

maybeCatchCorona :: Config -> StdGen -> Int -> Cell
maybeCatchCorona _ rand 0 = Cell Healthy rand
maybeCatchCorona conf rand n = do
    let (randNumbers, newRand) = getRandNumbers n rand
    if any (< probability conf) randNumbers then
      Cell (Infected (incubationPeriod conf)) newRand
    else
      Cell Healthy newRand

getRandNumbers :: Int -> StdGen -> ([Double], StdGen)
getRandNumbers n = runState (replicateM n (state random))

canSpread :: Cell -> Bool
canSpread c = case cState of
  (Infected _) -> True
  (Ill _)      -> True
  _            -> False
  where cState = cellState c

badNeighbourAmount :: Comonad19Grid -> Int
badNeighbourAmount g = length (filter (\neighbourFunc -> canSpread (extract (neighbourFunc g))) neighbors)

calcRule :: Comonad19Grid -> Config -> Cell
calcRule g conf = case cState of
  (Infected i) -> Cell (if i == 1 then Ill (illnessDuration conf) else Infected (i - 1)) rand
  (Ill i)      -> Cell (if i == 1 then Immune (immunityDuration conf) else Ill (i - 1)) rand
  (Immune i)   -> Cell (if i == 1 then Healthy else Immune (i - 1)) rand
  Healthy      -> maybeCatchCorona conf rand (badNeighbourAmount g)
  where c = extract g
        cState = cellState c
        rand  = cellRand c

rule :: Config -> Comonad19Grid -> Cell
rule conf g = calcRule g conf

showCell :: Cell -> Char
showCell c = case cState of
  (Infected _) -> '@'
  (Ill _)      -> '#'
  (Immune _)   -> '*'
  Healthy      -> '_'
  where cState = cellState c

showRow :: Int -> ListZipper Cell -> [Char]
showRow size lz = map showCell (toList lz size)

-- | Convert Grid to String for easy printing. 
-- Also takes an int - length of grid side
showGrid :: Int -> Comonad19Grid -> String
showGrid sideLen g = unlines (map (showRow half) (toList (unGrid g) half)) ++ "\n"
  where half = (sideLen - 1) `div` 2

startGrid :: Config -> StdGen -> Comonad19Grid
startGrid conf rand =
  gWrite (Cell (Infected days) rand) empty
    where empty = emptyGrid rand
          days = incubationPeriod conf

emptyRow :: StdGen -> ListZipper Cell
emptyRow rand = lGenerator (nextCell fst) (nextCell snd) (Cell Healthy rand)

emptyGrid :: StdGen -> Comonad19Grid
emptyGrid gen = Grid (lGenerator (nextRow fst) (nextRow snd) (emptyRow gen))

nextRow :: ((StdGen, StdGen) -> StdGen) -> ListZipper Cell -> ListZipper Cell
nextRow f lz = emptyRow (f (split (nextRand rand)))
  where rand = cellRand (extract lz)

nextCell :: ((StdGen, StdGen) -> StdGen) -> Cell -> Cell
nextCell f c = Cell Healthy (f (split rand))
  where rand = cellRand c

nextRand :: StdGen -> StdGen
nextRand rand = snd (random rand :: (Int, StdGen))

step :: Config -> Comonad19Grid -> Comonad19Grid
step conf = extend (rule conf)

-- | Creates a list of grids using the given configuration, starting stdGen and amount of steps.
-- Each element of this list represents one infection simulation step.
simulate :: Config -> StdGen -> Int -> [Comonad19Grid]
simulate conf rand steps = do
  let evolution = iterate (step conf) (startGrid conf rand)
  take steps evolution
