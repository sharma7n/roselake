module Graph exposing
  ( Graph
  , getNode
  , getAdjacentEdges
  , fake
  )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

import CustomDict exposing (CustomDict)

import SymmetricPair exposing (SymmetricPair)

type alias Graph label n e =
  { nodes : CustomDict label n
  , edges : CustomDict (SymmetricPair label) e
  }

fake : Graph label n e
fake =
  { nodes = CustomDict.new
  , edges = CustomDict.new
  }

getNode : (label -> String) -> label -> Graph label n e -> Maybe n
getNode labelToString label g =
  g.nodes
    |> CustomDict.get labelToString label

getAdjacentEdges : (label -> Encode.Value) -> Decoder label -> label -> Graph label n e -> List e
getAdjacentEdges labelEncoder labelDecoder label g =
  g.edges
    |> CustomDict.toList (SymmetricPair.fromString labelDecoder)
    |> List.filter (\(k, _) -> SymmetricPair.contains labelEncoder label k)
    |> List.map (\(_, v) -> v)