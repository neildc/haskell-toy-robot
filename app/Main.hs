module Main where

import           Control.Monad as Monad (when)
import qualified System.IO
import qualified Lib

debug :: Bool
debug = False -- TODO disable before final

debugLog :: String -> IO ()
debugLog =
  when debug . putStrLn

main :: IO ()
main = do
  isEOF <- System.IO.isEOF
  if isEOF then
    return ()

  else do
    line <- getLine

    -- The first valid command to the robot is a PLACE command, after that, any
    -- sequence of commands may be issued, in any order, including another PLACE
    -- command. The application should discard all commands in the sequence until
    -- a valid PLACE command has been executed.
    case (Lib.parse line >>= Lib.getStateIfPlaceCommand) of
      Right initialState ->
          loop initialState

      Left err ->
          do
            debugLog err
            main

loop :: Lib.State -> IO ()
loop currState = do
  isEOF <- System.IO.isEOF
  if isEOF then
    return ()

  else do
    line <- getLine

    if line == "REPORT" then
      do
        putStrLn $ Lib.printState currState
        loop currState

    else
      case Lib.parse line of
        Right command ->
          let
            updatedState = Lib.update command currState
          in
          do
            debugLog $ "Parsed command => " ++ show command
            debugLog $ "old state: " ++ show currState
            debugLog $ "new state: " ++ show updatedState
            loop updatedState
        Left err -> do
          debugLog $ "Failed to parse => " ++ line
          debugLog err
          loop currState
