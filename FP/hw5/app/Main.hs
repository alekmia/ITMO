module Main (main) where

import HW5.Parser
import HW5.Evaluator
import HW5.Pretty
import HW5.Action
import System.Console.Haskeline
import Data.Set
import Control.Monad.IO.Class

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just input -> do 
                            let parsed = parse input
                            case parsed of
                                Left _ -> outputStrLn "cannot parse" 
                                Right a -> do
                                    evaluated <- liftIO (runHIO (eval a) (fromList [AllowWrite, AllowRead, AllowTime]))
                                    case evaluated of
                                        Left _ -> outputStrLn "cannot eval"    
                                        Right val -> do
                                            outputStrLn (show (prettyValue val))
                            loop