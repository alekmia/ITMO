module Main (main) where

import Options.Applicative
import System.Random (newStdGen)

import HW6.T3

data Parameters = Parameters
  { prob       :: Double
  , incub      :: Int
  , ill        :: Int
  , immun      :: Int
  , gsize      :: Int
  , iterations :: Int
  }

paramParser :: Parser Parameters
paramParser = Parameters
      <$> option auto
          ( long "prob"
         <> help "probability of infection"
         <> showDefault
         <> value 1
         <> metavar "Double" )
      <*> option auto
          ( long "incub"
         <> help "incubation period"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> option auto
          ( long "ill"
         <> help "illness duration"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> option auto
          ( long "immun"
         <> help "immunity duration"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> option auto
          ( long "grid-size"
         <> help "grid size"
         <> showDefault
         <> value 11
         <> metavar "INT" )
      <*> option auto
          ( long "iterations"
         <> help "iterations amount"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = startGame =<< execParser opts
  where
    opts = info (paramParser <**> helper) fullDesc

startGame :: Parameters -> IO ()
startGame p = do
    let eligableParams = validate p
    case eligableParams of
        Nothing -> do
            newRand <- newStdGen
            let conf = Config (prob p) (incub p) (ill p) (immun p)
            let grids = simulate conf newRand (iterations p)
            mapM_ (putStrLn . showGrid (gsize p)) grids
        Just s -> putStrLn s

validate :: Parameters -> Maybe String
validate p 
  | even (gsize p)             = Just "pls use an odd size, as there is no 'center' on a square with an even side"
  | prob p <= 0 || prob p >= 1 = Just "probability should be in (0, 1)"
  | incub p < 1                = Just "incubation period should be atleast 1"
  | ill p < 1                  = Just "illness duration should be atleast 1"
  | immun p < 1                = Just "immunity duration should be atleast 1"
  | gsize p < 1                = Just "grid size should be atleast 1"
  | iterations p < 1           = Just "amount of iterations should be atleast 1"
  | otherwise                  = Nothing
        
