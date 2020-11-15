module Main where

import           Control.Monad as Monad (when)
import qualified Lib

debug :: Bool
debug = True -- TODO disable before final

main :: IO ()
main = do
    line <- getLine

    -- The first valid command to the robot is a PLACE command, after that, any
    -- sequence of commands may be issued, in any order, including another PLACE
    -- command. The application should discard all commands in the sequence until
    -- a valid PLACE command has been executed.
    case (Lib.parse line >>= Lib.getStateIfPlaceCommand) of
      Just initialState ->
          loop initialState

      Nothing ->
          main

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
