module Lib
    ( State, Direction(..)
    , update, parse
    , boardHeight, boardWidth
    ) where

boardHeight :: Int
boardHeight = 5

boardWidth :: Int
boardWidth = 5

type State = (Position, Direction)

type Position = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq)

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
