module CustomDict exposing
  ( CustomDict
  , new
  , insert
  , get
  , member
  , keys
  , toList
  )

import Dict exposing (Dict)

type CustomDict a b
  = T (Dict String b)

new : CustomDict a b
new =
  T <| Dict.empty

insert : (a -> String) -> a -> b -> CustomDict a b -> CustomDict a b
insert aToString key val (T d) =
  d
    |> Dict.insert (aToString key) val
    |> T

get : (a -> String) -> a -> CustomDict a b -> Maybe b
get aToString key (T d) =
  d
    |> Dict.get (aToString key)

keys : (String -> Maybe a) -> CustomDict a b -> List a
keys aFromString (T d) =
  d
    |> Dict.keys
    |> List.filterMap aFromString

toList : (String -> Maybe a) -> CustomDict a b -> List ( a, b )
toList aFromString (T d) =
  d
    |> Dict.toList
    |> List.filterMap (\(k, v) ->
      case aFromString k of
        Just a ->
          Just (a, v)
        
        Nothing ->
          Nothing
    )

member : (a -> String) -> a -> CustomDict a b -> Bool
member aToString a (T d) =
  d
    |> Dict.member (aToString a)