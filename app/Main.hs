module Main where

import Lib

main :: IO ()
main = loop ((0,0), North)

loop :: State -> IO ()
loop currState = do
    line <- getLine
    case parse line of
      Just command ->
        loop $ update command currState
      Nothing ->
        loop currState

type State = (Position, Direction)

type Position = (Int, Int)

data Direction = North | East | South | West deriving (Show)

data Command
  = Place Position Direction
  | Move
  | RotateLeft
  | RotateRight
  | Report
  deriving (Show)

update :: Command -> State -> State
update command currState =
  currState

parse :: String -> Maybe Command
parse input =
  undefined
