module CustomDict exposing
  ( CustomDict
  , insert
  , get
  , member
  )

import Dict exposing (Dict)

type CustomDict a b
  = T (Data a b)

type alias Data a b =
  { toString : a -> String
  , inner : Dict String b
  }

new : (a -> String) -> CustomDict a b
new toString =
  { toString = toString
  , inner = Dict.empty
  }
    |> T

insert : a -> b -> CustomDict a b -> CustomDict a b
insert key val (T d) =
  let
    newInner =
      d.inner
        |> Dict.insert (d.toString key) val
  in
  { d |
    inner = newInner
  }
    |> T

get : a -> CustomDict a b -> Maybe b
get key (T d) =
  d
    |> Dict.get (d.toString key)

member : a -> CustomDict a b -> Maybe b
member key (T d) =
  d
    |> Dict.member (d.toString key)