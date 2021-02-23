module Graph exposing
  ( Graph
  , getNode
  , getAdjacentEdges
  , fake
  , describeAt
  )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

import CustomDict exposing (CustomDict)
import Util

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

getNode : (label -> Encode.Value) -> label -> Graph label n e -> Maybe n
getNode labelEncoder label g =
  g.nodes
    |> CustomDict.get labelEncoder label

getAdjacentEdges : (label -> Encode.Value) -> Decoder label -> label -> Graph label n e -> List e
getAdjacentEdges labelEncoder labelDecoder label g =
  g.edges
    |> CustomDict.toList (SymmetricPair.decoder labelDecoder)
    |> List.filter (\(k, _) -> SymmetricPair.contains labelEncoder label k)
    |> List.map (\(_, v) -> v)

describeAt : (label -> Encode.Value) -> Decoder label -> (n -> a) -> (e -> a) -> label -> Graph label n e -> List a
describeAt labelEncoder labelDecoder fromNode fromEdge label g =
  let
    nodeDescriptors =
      g
        |> getNode labelEncoder label
        |> Util.maybeToList
        |> List.map fromNode
    
    edgeDescriptors =
      g
        |> getAdjacentEdges labelEncoder labelDecoder label
        |> List.map fromEdge
  in
  nodeDescriptors ++ edgeDescriptors
  
