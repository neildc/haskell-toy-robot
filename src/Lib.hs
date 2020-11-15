module Lib
    ( State, Direction(..)
    , update, parse, printState, getStateIfPlaceCommand
    , boardHeight, boardWidth
    ) where

import qualified Text.Ascii    as Ascii
import qualified Data.List as List
import qualified Data.Ix
import qualified Control.Applicative

boardHeight :: Int
boardHeight = 5

boardWidth :: Int
boardWidth = 5

type State = (Position, Direction)

printState :: State -> String
printState ((x,y),dir) =
  List.intercalate ","
    [ show x
    , show y
    , map Ascii.toUpper $ show dir
    ]

type Position = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq)

data Command
  = Place Position Direction
  | Move
  | RotateLeft
  | RotateRight
  deriving (Show)

getStateIfPlaceCommand :: Command -> Maybe State
getStateIfPlaceCommand (Place pos dir) = Just (pos, dir)
getStateIfPlaceCommand _               = Nothing

-- Keeping "REPORT" out of Lib.hs to keep the
-- update function pure, so it can be used in the test runner
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
      (moveIfRobotWontFall currState, currDirection)

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

moveIfRobotWontFall :: State -> Position
moveIfRobotWontFall (currPosition, currDirection) =
  let
      (willFall, newPosition) =
        case currDirection of
          North ->
            ( y == (boardHeight - 1)
            , (x, y + 1)
            )

          South ->
            ( y == 0
            , (x, y - 1)
            )

          East ->
            ( x == (boardWidth - 1)
            , (x + 1, y)
            )

          West ->
            ( x == 0
            , (x - 1, y)
            )

      ( x, y ) =
        currPosition
  in
  if willFall then
    currPosition

  else
    newPosition

parse :: String -> Maybe Command
parse input =
  case input of
    "LEFT" -> Just RotateLeft
    "RIGHT" -> Just RotateRight
    "MOVE" -> Just Move
    _ ->
        List.stripPrefix "PLACE " input
          >>= splitPlaceArgsOnComma
          >>= parsePlaceArgs
  where
    splitPlaceArgsOnComma :: String -> Maybe (Char, Char, String)
    splitPlaceArgsOnComma input =
      case input of
          (x:',':y:',':direction) ->
            Just (x, y, direction)

          _ ->
            Nothing

    parsePlaceArgs :: (Char, Char, String) -> Maybe Command
    parsePlaceArgs (x, y, direction) =
      Control.Applicative.liftA3
          ( \validX validY validDir -> Place (validX, validY) validDir)
          ( Ascii.fromDecDigit x >>= isInValidRange (0, boardWidth - 1))
          ( Ascii.fromDecDigit y >>= isInValidRange (0, boardHeight - 1))
          ( parseDirection direction)

    parseDirection :: String -> Maybe Direction
    parseDirection input =
      case input of
        "NORTH" -> Just North
        "EAST"  -> Just East
        "SOUTH" -> Just South
        "WEST"  -> Just West
        _       -> Nothing

    isInValidRange :: (Int, Int) -> Int -> Maybe Int
    isInValidRange bounds value =
      if Data.Ix.inRange bounds value then
        Just value
      else
        Nothing
