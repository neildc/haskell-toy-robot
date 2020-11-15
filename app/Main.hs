module Main where

import qualified Lib

main :: IO ()
main = loop ((0,0), Lib.North)

loop :: Lib.State -> IO ()
loop currState = do
    line <- getLine
    case Lib.parse line of
      Just command ->
        loop $ Lib.update command currState
      Nothing ->
        loop currState

