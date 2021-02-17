module Grid exposing
  ( Grid
  , fake
  )

import Coordinates exposing (Coordinates)
import Graph exposing (Graph)
import Direction exposing (Direction)

type alias Grid n e =
  Graph Coordinates n e

fake : Grid n e
fake =
  Graph.fake