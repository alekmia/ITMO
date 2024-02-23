module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day
afterDays i d
  | i == 0    = d
  | otherwise = afterDays (i - 1) (nextDay d)

isWeekend :: Day -> Bool
isWeekend d = case d of
  Saturday -> True
  Sunday -> True
  _ -> False

daysToParty :: Day -> Natural
daysToParty d = case d of
  Friday -> 0
  _ -> daysToParty (nextDay d) + 1
