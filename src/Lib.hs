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
  let
    (currPosition, currDirection) =
      currState
  in
  case command of
    Place position direction ->
      undefined

    Move ->
      undefined

    RotateLeft ->
      ( currPosition
      , case currDirection of
        North -> West
        East  -> North
        South -> East
        West  -> South
      )

    RotateRight ->
      ( currPosition
      , case currDirection of
        North -> East
        East  -> South
        South -> West
        West  -> North
      )

    Report ->
      undefined

parse :: String -> Maybe Command
parse input =
  case input of
    "LEFT" -> Just RotateLeft
    "RIGHT" -> Just RotateRight
    "MOVE" -> Just Move
    "Report" -> Just Report
    _ ->
      -- TODO parse PLACE X,Y,[NORTH | EAST | SOUTH | WEST]
      Nothing
