module Grid exposing
  ( Grid
  )

import Coordinates exposing (Coordinates)
import Graph exposing (Graph)

type alias Grid n e =
  Graph Coordinates n e