module Graph exposing
  ( Graph
  , isConnected
  )

import CustomDict exposing (CustomDict)

import SymmetricPair exposing (SymmetricPair)

type alias Graph label n e =
  { nodes : CustomDict label n
  , edges : CustomDict (SymmetricPair label) e
  }

isTwoNodesConnected : label -> label -> Graph label n e -> Bool
isTwoNodesConnected _ _ _ =
  -- TODO
  True

isConnected : (label -> String) -> Graph label n e -> Bool
isConnected labelToString g =
  g.nodes
    |> CustomDict.keys
    |> SymmetricPair.distinctPairs
    |> List.all (\p ->
      let
        ( n1, n2 ) =
          SymmetricPair.toPair labelToString p
      in
      isTwoNodesConnected n1 n2 g
    )