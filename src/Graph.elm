module Graph exposing
  ( Graph
  , getNode
  , getEdges
  )

import CustomDict exposing (CustomDict)

import SymmetricPair exposing (SymmetricPair)

type alias Graph label n e =
  { nodes : CustomDict label n
  , edges : CustomDict (SymmetricPair label) e
  }

getNode : (label -> String) -> label -> Graph label n e -> Maybe n
getNode labelToString label g =
  g.nodes
    |> CustomDict.get labelToString label

getEdges : (label -> String) -> label -> Graph label n e -> List e
getEdges labelToString label g =
  []