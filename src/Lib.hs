module Lib
    ( State, Direction(..)
    , update, parse, stateToString, getStateIfPlaceCommand
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

type State = ( Position -- robot
             , Direction -- robot direction
             , [ Position ] -- obstacles on the board
             )

stateToString :: State -> String
stateToString ((x,y),dir, _) =
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
  | PlaceObject
  deriving (Show)

-- used to initialise the state
getStateIfPlaceCommand :: Command -> Either String State
getStateIfPlaceCommand (Place pos dir) = Right (pos, dir, [])
getStateIfPlaceCommand _               = Left "getStateIfPlaceCommand: expected PLACE command"

-- Keeping "REPORT" out of Lib.hs to keep the
-- update function pure, so it can be used in the test runner
update :: Command -> State -> State
update command currState =
  let
    (currPosition, currDirection, currObstacles) =
      currState
  in
  case command of
    Place newPosition newDirection ->

      -- if placing on position where there is an obstacle
      ----- overwrite obstacle
      ----- ignore place command

      -- For simplicty simply resetting obstacle state
      ( newPosition, newDirection, [] )

    Move ->
      (moveIfRobotWontFall currState, currDirection, currObstacles)

    RotateLeft ->
      ( currPosition
      , case currDirection of
        North -> West
        East  -> North
        South -> East
        West  -> South
      , currObstacles
      )

    RotateRight ->
      ( currPosition
      , case currDirection of
        North -> East
        East  -> South
        South -> West
        West  -> North
      , currObstacles
      )

    PlaceObject ->
      ( currPosition
      , currDirection
      , placeObsIfNotFacingEdge currState
      )

placeObsIfNotFacingEdge :: State -> [Position]
placeObsIfNotFacingEdge (currPosition, currDirection, currObstacles) =
  let
      (facingEdge, newObs) =
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
  -- TODO Check for obstacles
  if facingEdge then
    currObstacles

  else
    if List.elem newObs currObstacles then
      currObstacles
    else
      List.insert newObs currObstacles


moveIfRobotWontFall :: State -> Position
moveIfRobotWontFall (currPosition, currDirection, currObstacles) =
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

      obsInFront =
        List.elem newPosition currObstacles
  in
  -- TODO Check for obstacles
  if willFall || obsInFront then
    currPosition

  else
    newPosition

parse :: String -> Either String Command
parse input =
  case input of
    "LEFT" -> Right RotateLeft
    "RIGHT" -> Right RotateRight
    "MOVE" -> Right Move
    "PLACE_OBJECT" -> Right PlaceObject
    _ ->
      case List.stripPrefix "PLACE " input of
        Nothing ->
          Left $ errorStr "attempted to parse unknown command"
        Just placeArgs ->
          splitPlaceArgsOnComma placeArgs
            >>= parsePlaceArgs
  where
    errorStr desc =
      "parse: " ++ desc ++ " => \"" ++ input ++ "\""

    splitPlaceArgsOnComma :: String -> Either String (Char, Char, String)
    splitPlaceArgsOnComma argStr =
      case argStr of
          (x:',':y:',':direction) ->
            Right (x, y, direction)

          _ ->
            Left $ errorStr "Failed splitting PLACE args"

    parsePlaceArgs :: (Char, Char, String) -> Either String Command
    parsePlaceArgs (x, y, direction) =
      Control.Applicative.liftA3
          ( \validX validY validDir -> Place (validX, validY) validDir)
          ( parseDigit x >>= isInValidRange (0, boardWidth - 1))
          ( parseDigit y >>= isInValidRange (0, boardHeight - 1))

          ( parseDirection direction)
    parseDirection :: String -> Either String Direction
    parseDirection dirStr =
      case dirStr of
        "NORTH" -> Right North
        "EAST"  -> Right East
        "SOUTH" -> Right South
        "WEST"  -> Right West
        _       -> Left $ errorStr "attempted to parse invalid direction arg"

    parseDigit :: Char -> Either String Int
    parseDigit digit =
      case Ascii.fromDecDigit digit of
        Just d -> Right d
        Nothing -> Left $ errorStr "attempted to parse non digit coordinate"

    isInValidRange :: (Int, Int) -> Int -> Either String Int
    isInValidRange bounds value =
      if Data.Ix.inRange bounds value then
        Right value
      else
        Left $ errorStr "PLACE coordinate was out of bounds"
