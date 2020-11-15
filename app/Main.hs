module Main where

import qualified Lib
import Control.Monad as Monad (when)

debug :: Bool
debug = True -- TODO disable before final

main :: IO ()
main = loop ((0,0), Lib.North)

loop :: Lib.State -> IO ()
loop currState = do
    line <- getLine

    if line == "REPORT" then
      do
        putStrLn $ Lib.printState currState
        loop currState

    else
      case Lib.parse line of
        Just command ->
          let
            updatedState = Lib.update command currState
          in
          do
            when debug $ putStrLn $ "Parsed command => " ++ show command
            when debug $ putStrLn $ "old state: " ++ show currState
            when debug $ putStrLn $ "new state: " ++ show updatedState
            loop updatedState
        Nothing -> do
          when debug $ putStrLn $ "Failed to parse => " ++ line
          loop currState
