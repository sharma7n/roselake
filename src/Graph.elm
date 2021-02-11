module Graph exposing
  (
  )

import CustomDict exposing (CustomDict)

import SymmetricPair exposing (SymmetricPair)

type alias Graph label n e =
  { nodes : CustomDict label n
  , edges : CustomDict (SymmetricPair label) e
  }