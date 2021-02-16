module Grid exposing
  ( Grid
  , moveNorth
  , moveEast
  , moveSouth
  , moveWest
  )

import Coordinates exposing (Coordinates)
import Graph exposing (Graph)
import Direction exposing (Direction)

type alias Grid n e =
  { currentPosition : Coordinates
  , graph : Graph Coordinates n e
  }

moveDirection : Direction -> Grid n e -> Grid n e
moveDirection dir g =
  let
    newCoordinates =
      grid.currentPosition
        |> Coordinates.move dir
    
    newMaybeNode =
      g.graph
        |> Graph.getNode Coordinates.toString newCoordinates
    
    nextPosition =
      case newMaybeNode of
        Just newNode ->
          newCoordinates
        
        Nothing ->
          grid.currentPosition
  in
  { g | currentPosition = nextPosition
  }

moveNorth : Grid n e -> Grid n e
moveNorth =
  moveDirection Direction.North

moveEast : Grid n e -> Grid n e
moveEast =
  moveDirection Direction.East

moveSouth : Grid n e -> Grid n e
moveSouth =
  moveDirection Direction.South

moveWest : Grid n e -> Grid n e
moveWest =
  moveDirection Direction.West