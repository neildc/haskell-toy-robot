module Lib
    ( State, Direction(..)
    , update, parse
    , boardHeight, boardWidth
    ) where

import qualified Text.Ascii    as Ascii
import qualified Data.List as List

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
    Place newPosition newDirection ->
      ( newPosition, newDirection )

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
    "REPORT" -> Just Report
    _ ->
        case List.stripPrefix "PLACE " input of
          Just (x:',':y:',':direction) ->
            parsePlaceArgs x y direction

          _ ->
            Nothing
  where
    parsePlaceArgs :: Char -> Char -> String -> Maybe Command
    parsePlaceArgs x y direction =
      case ( Ascii.fromDecDigit x
           , Ascii.fromDecDigit y
           , parseDirection direction
           ) of
        (Just parsedX, Just parsedY, Just parsedDir) ->
          Just $ Place (parsedX, parsedY) parsedDir
        _ ->
          Nothing

    parseDirection :: String -> Maybe Direction
    parseDirection input =
      case input of
        "NORTH" -> Just North
        "EAST"  -> Just East
        "SOUTH" -> Just South
        "WEST"  -> Just West
        _       -> Nothing
