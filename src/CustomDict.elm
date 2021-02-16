module CustomDict exposing
  ( CustomDict
  , insert
  , get
  , member
  , keys
  )

import Dict exposing (Dict)

type CustomDict a b
  = T (Dict String b)

new : CustomDict a b
new =
  T <| Dict.empty

insert : (a -> String) -> a -> b -> CustomDict a b -> CustomDict a b
insert aToString key val (T d) =
  let
    newInner =
      d.inner
        |> Dict.insert (aToString key) val
  in
  { d |
    inner = newInner
  }
    |> T

get : (a -> String) -> a -> CustomDict a b -> Maybe b
get aToString key (T d) =
  d
    |> Dict.get (aToString key)

member : (a -> String) -> a -> CustomDict a b -> Maybe b
member aToString key (T d) =
  d
    |> Dict.member (aToString key)

keys : (String -> Maybe a) -> CustomDict a b -> List a
keys aFromString (T d) =
  d
    |> Dict.keys
    |> List.filterMap aFromString