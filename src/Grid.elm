module Grid exposing
  ( Grid
  , fake
  , describeAt
  )

import Coordinates exposing (Coordinates)
import CustomDict exposing (CustomDict)
import Graph exposing (Graph)
import Direction exposing (Direction)
import SymmetricPair exposing (SymmetricPair)

type alias Grid n e =
  Graph Coordinates n e

fake : n -> n -> e -> Grid n e
fake n1 n2 e =
  let
    point1 =
      Coordinates.fake
    
    point2 =
      point1
        |> Coordinates.move Direction.North
    
    nodes =
      CustomDict.new
        |> CustomDict.insert Coordinates.encoder point1 n1
        |> CustomDict.insert Coordinates.encoder point2 n2
    
    edgeLabel =
      SymmetricPair.new Coordinates.encoder point1 point2
    
    edges =
      CustomDict.new
        |> CustomDict.insert (SymmetricPair.encoder Coordinates.encoder) edgeLabel e
  in
  { nodes = nodes
  , edges = edges
  }

describeAt : (n -> a) -> (e -> a) -> Coordinates -> Grid n e -> List a
describeAt fromNode fromEdge coords g =
  Graph.describeAt
    Coordinates.encoder
    Coordinates.decoder
    fromNode
    fromEdge
    coords
    g